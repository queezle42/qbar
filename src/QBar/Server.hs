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

import Control.Monad (when, unless, forM_)
import Control.Concurrent.Async (async, link)
import Control.Concurrent.Event as Event
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar, modifyMVar_)
import Control.Exception (throw)
import Data.Aeson (encode, decode, ToJSON, toJSON, object, (.=))
import Data.ByteString.Lazy (hPut)
import qualified Data.ByteString.Char8 as BSSC8
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as T
import Pipes
import Pipes.Concurrent (Input, spawn, latest, toOutput, fromInput)
import qualified Pipes.Prelude as PP
import System.IO (stdin, stdout, stderr, hFlush)

renderIndicators :: [Text]
renderIndicators = ["*"] <> cycle ["/", "-", "\\", "|"]

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
swayBarInput :: MainOptions -> Producer BlockEvent IO ()
swayBarInput MainOptions{verbose} = swayBarInput'
  where
    swayBarInput' :: Producer BlockEvent IO ()
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


swayBarOutput :: MainOptions -> Consumer [ThemedBlockOutput] IO ()
swayBarOutput options@MainOptions{indicator} = do
  -- Print header
  liftIO $ do
    putStrLn "{\"version\":1,\"click_events\":true}"
    putStrLn "["

  if indicator
    then swayBarOutputWithIndicator' renderIndicators
    else swayBarOutput'
  where
    swayBarOutput' :: Consumer [ThemedBlockOutput] IO ()
    swayBarOutput' = do
      blockOutputs <- await
      liftIO $ outputLine options blockOutputs
      swayBarOutput'
    swayBarOutputWithIndicator' :: [Text] -> Consumer [ThemedBlockOutput] IO ()
    swayBarOutputWithIndicator' [] = throw $ userError "List should be infinite"
    swayBarOutputWithIndicator' (ind : inds) = do
      blockOutputs <- await
      liftIO $ outputLine options (blockOutputs <> [whiteThemedBlockOutput ind])
      swayBarOutputWithIndicator' inds
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
    encodeOutput :: [ThemedBlockOutput] -> BS.ByteString
    encodeOutput blocks = encode $ map renderPangoBlock $ blocks
    renderPangoBlock :: ThemedBlockOutput -> PangoBlock
    renderPangoBlock ThemedBlockOutput{_fullText, _shortText, _blockName} = PangoBlock {
      pangoBlockFullText = renderPango _fullText,
      pangoBlockShortText = renderPango <$> _shortText,
      pangoBlockName = _blockName
    }

runBarServer :: MainOptions -> BarIO () -> IO ()
runBarServer options = runBarHost barServer
  where
    barServer :: BarIO (Consumer [BlockOutput] IO (), Producer BlockEvent IO ())
    barServer = do
      -- Event to render the bar (fired when block output or theme is changed)
      renderEvent <- liftIO Event.new

      -- Mailbox to store the latest 'BlockOutput's
      (output, input) <- liftIO $ spawn $ latest []

      -- MVar that holds the current theme, linked to the input from the above mailbox
      (themedBlockProducerMVar :: MVar (Producer [ThemedBlockOutput] IO (), Bool)) <- liftIO $ newMVar $ throw $ userError "Unexpected behavior: Default theme not set"

      let setTheme' = setTheme renderEvent input themedBlockProducerMVar

      -- Set default theme
      liftIO $ setTheme' defaultTheme

      bar <- askBar

      -- Create control socket
      controlSocketAsync <- liftIO $ listenUnixSocketAsync options bar (commandHandler setTheme')
      liftIO $ link controlSocketAsync


      -- Run render loop
      liftIO $ link =<< async (renderLoop renderEvent themedBlockProducerMVar)

      -- Return a consumer that accepts BlockOutputs from the bar host, moves them to the mailbox and signals the renderer to update the bar.
      return (signalPipe renderEvent >-> toOutput output, swayBarInput options)

    renderLoop :: Event.Event -> MVar (Producer [ThemedBlockOutput] IO (), Bool) -> IO ()
    renderLoop renderEvent themedBlockProducerMVar = runEffect $
      themeAnimator renderEvent themedBlockProducerMVar >-> filterDuplicates >-> swayBarOutput options

    themeAnimator :: Event.Event -> MVar (Producer [ThemedBlockOutput] IO (), Bool) -> Producer [ThemedBlockOutput] IO ()
    themeAnimator renderEvent themedBlockProducerMVar = themeAnimator'
      where
        themeAnimator' :: Producer [ThemedBlockOutput] IO ()
        themeAnimator' = do
          (themedBlocks, isAnimated'') <- liftIO $ modifyMVar themedBlockProducerMVar (\(themedBlockProducer, isAnimated') -> do
            result <- next themedBlockProducer
            case result of
              -- TODO: fix type safety on this somehow?
              Left _ -> throw $ userError "Unexpected behavior: Themes and output cache mailbox should never return"
              Right (themedBlocks, nextThemedBlockProducer) ->
                return ((nextThemedBlockProducer, isAnimated'), (themedBlocks, isAnimated'))
            )
          yield themedBlocks
          liftIO $ if isAnimated''
            -- Limit to 10 FPS because swaybar rendering is surprisingly expensive
            -- TODO: make FPS configurable
            then void $ Event.waitTimeout renderEvent 100000
            else Event.wait renderEvent
          themeAnimator'

    setTheme :: Event.Event -> Input [BlockOutput] -> MVar (Producer [ThemedBlockOutput] IO (), Bool) -> Theme -> IO ()
    setTheme renderEvent blockOutputInput themedBlockProducerMVar theme = do
      modifyMVar_ themedBlockProducerMVar (\_ -> return (mkThemedBlockProducer theme))
      Event.signal renderEvent
      where
        mkThemedBlockProducer :: Theme -> (Producer [ThemedBlockOutput] IO (), Bool)
        mkThemedBlockProducer (StaticTheme themeFn) = (fromInput blockOutputInput >-> PP.map themeFn, False)
        mkThemedBlockProducer (AnimatedTheme themePipe) = (fromInput blockOutputInput >-> themePipe, True)

    commandHandler :: (Theme -> IO ()) -> Command -> IO CommandResult
    commandHandler setTheme' (SetTheme name) =
      case findTheme name of
        Left err -> return $ Error err
        Right theme -> do
          setTheme' theme
          return Success


-- |Entry point.
runQBar :: BarIO () -> MainOptions -> IO ()
runQBar barConfiguration options@MainOptions{barCommand} = runCommand barCommand
  where
    runCommand BarServerCommand = runBarServer options barConfiguration
    runCommand ConnectSocket = sendBlockStream options barConfiguration
    runCommand (SetThemeCommand themeName) = sendIpc options $ SetTheme themeName
