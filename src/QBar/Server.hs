{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module QBar.Server where

import QBar.BlockOutput
import QBar.Core
import QBar.Cli
import QBar.ControlSocket
import QBar.Host
import QBar.Pango
import QBar.Theme
import QBar.Util

import Control.Monad (forever, when, unless, forM_)
import Control.Concurrent.Async (async, link)
import Control.Concurrent.Event as Event
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar, modifyMVar_)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (readTChan)
import Control.Exception (throw)
import Data.Aeson (encode, decode, ToJSON, toJSON, object, (.=))
import Data.ByteString.Lazy (hPut)
import qualified Data.ByteString.Char8 as BSSC8
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import Pipes
import Pipes.Concurrent (Input, spawn, latest, toOutput, fromInput)
import qualified Pipes.Prelude as PP
import System.IO (stdin, stdout, stderr, hFlush)

renderIndicator :: CachedBlock
-- Using 'cachedBlock' is a hack to actually get the block to update on every bar update (by doing this it will not get a cache later in the pipeline).
renderIndicator = forever $ each $ map (mkBlockState . mkBlockOutput . normalText) ["/", "-", "\\", "|"]

data PangoBlock = PangoBlock {
  pangoBlockFullText :: PangoText,
  pangoBlockShortText :: Maybe PangoText,
  pangoBlockName :: Maybe T.Text
} deriving(Show)
instance ToJSON PangoBlock where
  toJSON PangoBlock{pangoBlockFullText, pangoBlockShortText, pangoBlockName} = object $
    fullText' <> shortText' <> blockName' <> pango'
    where
      fullText' = [ "full_text" .= pangoBlockFullText ]
      shortText' = fromMaybe (\s -> ["short_text" .= s]) mempty pangoBlockShortText
      blockName' = fromMaybe (\s -> ["name" .= s]) mempty pangoBlockName
      pango' = [ "markup" .= ("pango" :: T.Text) ]


-- |A producer that reads swaybar/i3bar-input events from stdin and emits them as 'BlockEvent's.
swayBarInput :: MainOptions -> Producer BlockEvent BarIO ()
swayBarInput MainOptions{verbose} = swayBarInput'
  where
    swayBarInput' :: Producer BlockEvent BarIO ()
    swayBarInput' = do
      line <- liftIO $ BSSC8.hGetLine stdin

      unless (line == "[") $ do
        -- Echo input to stderr when verbose flag is set
        when verbose $ liftIO $ do
          liftIO $ BSSC8.hPutStrLn stderr line
          hFlush stderr

        let maybeBlockEvent = decode $ removeComma $ BS.fromStrict line
        forM_ maybeBlockEvent yield

      swayBarInput'

    removeComma :: C8.ByteString -> C8.ByteString
    removeComma line
      | C8.head line == ',' = C8.tail line
      | C8.last line == ',' = C8.init line
      | otherwise = line


outputLine :: MainOptions -> [ThemedBlockOutput] -> IO ()
outputLine MainOptions{verbose} themedBlocks = do
  let encodedOutput = encodeOutput themedBlocks

  liftIO $ do
    hPut stdout encodedOutput
    putStrLn ","
    hFlush stdout
    -- Echo output to stderr when verbose flag is set
    when verbose $ do
      hPut stderr encodedOutput
      hPut stderr "\n"
      hFlush stderr
  where
    encodeOutput :: [ThemedBlockOutput] -> BS.ByteString
    encodeOutput blocks = encode $ map renderPangoBlock $ blocks
    renderPangoBlock :: ThemedBlockOutput -> PangoBlock
    renderPangoBlock ThemedBlockOutput{_fullText, _shortText, _blockName} = PangoBlock {
      pangoBlockFullText = renderPango _fullText,
      pangoBlockShortText = renderPango <$> _shortText,
      pangoBlockName = _blockName
    }

runBarServer :: BarIO () -> MainOptions -> IO ()
runBarServer defaultBarConfig options = runBarHost barServer (swayBarInput options)
  where
    barServer :: Consumer [BlockOutput] BarIO ()
    barServer = do
      -- Load blocks
      lift $ do
        when (indicator options) $ addBlock renderIndicator
        defaultBarConfig


      -- Event to render the bar (fired when block output or theme is changed)
      renderEvent <- liftIO Event.new

      -- Mailbox to store the latest 'BlockOutput's
      (output, input) <- liftIO $ spawn $ latest []

      -- MVar that holds the current theme, linked to the input from the above mailbox
      (themedBlockProducerMVar :: MVar (Producer [ThemedBlockOutput] IO (), Bool)) <- liftIO $ newMVar $ (return (), False)


      -- Create control socket
      commandChan <- liftIO createCommandChan
      controlSocketAsync <- liftIO $ listenUnixSocketAsync options commandChan
      liftIO $ link controlSocketAsync

      -- Update bar on control socket messages
      socketUpdateAsync <- liftIO $ async $ forever $ do
        command <- atomically $ readTChan commandChan
        case command of
          SetTheme name -> do
            let result = findTheme name
            case result of
              Left err -> TIO.hPutStrLn stderr err
              Right theme -> do
                setTheme input themedBlockProducerMVar theme
                Event.signal renderEvent
      liftIO $ link socketUpdateAsync

      liftIO $ do
        -- Set default theme
        setTheme input themedBlockProducerMVar defaultTheme

        -- Print header
        putStrLn "{\"version\":1,\"click_events\":true}"
        putStrLn "["
        -- Run render loop
        liftIO $ link =<< async (renderLoop renderEvent themedBlockProducerMVar)

      -- Return a consumer that accepts BlockOutputs from the bar host, moves them to the mailbox and signals the renderer to update the bar.
      signalPipe renderEvent >-> toOutput output

    renderLoop :: Event.Event -> MVar (Producer [ThemedBlockOutput] IO (), Bool) -> IO ()
    renderLoop renderEvent themedBlockProducerMVar = forever $ do
      (themedBlocks, isAnimated'') <- modifyMVar themedBlockProducerMVar (\(themedBlockProducer, isAnimated') -> do
        result <- next themedBlockProducer
        case result of
          -- TODO: fix type safety on this somehow?
          Left _ -> throw $ userError "Unexpected behavior: themes and mailboxes should never return"
          Right (themedBlocks, nextThemedBlockProducer) ->
            return ((nextThemedBlockProducer, isAnimated'), (themedBlocks, isAnimated'))
        )
      outputLine options themedBlocks
      if isAnimated''
        -- Limit to 10 FPS because swaybar rendering is surprisingly expensive
        -- TODO: make FPS configurable
        then void $ Event.waitTimeout renderEvent 100000
        else Event.wait renderEvent

    setTheme :: Input [BlockOutput] -> MVar (Producer [ThemedBlockOutput] IO (), Bool) -> Theme -> IO ()
    setTheme blockOutputInput themedBlockProducerMVar (StaticTheme theme) =
      modifyMVar_ themedBlockProducerMVar (\_ -> return (fromInput blockOutputInput >-> PP.map theme, False))
    setTheme blockOutputInput themedBlockProducerMVar (AnimatedTheme theme) =
      modifyMVar_ themedBlockProducerMVar (\_ -> return (fromInput blockOutputInput >-> theme, True))


-- |Entry point.
runQBar :: BarIO () -> MainOptions -> IO ()
runQBar barConfiguration options@MainOptions{barCommand} = runCommand barCommand
  where
    runCommand BarServer = runBarServer barConfiguration options
    runCommand DefaultTheme = sendIpc options $ SetTheme "default"
    runCommand RainbowTheme = sendIpc options $ SetTheme "rainbow"
