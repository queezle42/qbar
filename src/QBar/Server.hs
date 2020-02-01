{-# LANGUAGE DuplicateRecordFields #-}

module QBar.Server where

import QBar.BlockOutput
import QBar.Core
import QBar.Cli
import QBar.ControlSocket
import QBar.Host
import QBar.Theme

import Control.Monad (forever, when, unless, forM_)
import Control.Concurrent.Async
import Control.Concurrent.STM.TChan (newTChanIO)
import Data.Aeson (encode, decode, ToJSON, toJSON, object, (.=))
import Data.ByteString.Lazy (hPut)
import qualified Data.ByteString.Char8 as BSSC8
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as T
import Pipes
import System.IO (stdin, stdout, stderr, hFlush)
import Control.Lens hiding (each, (.=))

renderIndicator :: CachedBlock
-- Using 'cachedBlock' is a hack to actually get the block to update on every bar update (by doing this it will not get a cache later in the pipeline).
renderIndicator = forever $ each $ map (mkBlockState . mkBlockOutput . normalText) ["/", "-", "\\", "|"]

data RenderBlock = RenderBlock T.Text (Maybe T.Text) (Maybe T.Text)
  deriving(Show)
instance ToJSON RenderBlock where
  toJSON (RenderBlock fullText' shortText' blockName') = object $
    fullText'' <> shortText'' <> blockName'' <> pango''
    where
      fullText'' = [ "full_text" .= fullText' ]
      shortText'' = fromMaybe (\s -> ["short_text" .= s]) mempty shortText'
      blockName'' = fromMaybe (\s -> ["name" .= s]) mempty blockName'
      pango'' = [ "markup" .= ("pango" :: T.Text) ]


-- |A consumer that accepts lists of 'BlockOutput' and renders them to stdout using the {sway,i3}bar-protocol.
swayBarOutput :: MainOptions -> Consumer [BlockOutput] BarIO ()
swayBarOutput MainOptions{verbose} = do
  -- Output header
  liftIO $ do
    putStrLn "{\"version\":1,\"click_events\":true}"
    putStrLn "["

  swayBarOutput'
  where
    swayBarOutput' :: Consumer [BlockOutput] BarIO ()
    swayBarOutput' = do
      blocks <- await

      let encodedOutput = encodeOutput blocks

      liftIO $ do
        hPut stdout encodedOutput
        putStrLn ","
        hFlush stdout
        -- Echo output to stderr when verbose flag is set
        when verbose $ do
          hPut stderr encodedOutput
          hPut stderr "\n"
          hFlush stderr

      swayBarOutput'
    encodeOutput :: [BlockOutput] -> BS.ByteString
    encodeOutput bs = encode $ zipWith encodeBlock bs $ defaultTheme bs
    encodeBlock :: BlockOutput -> (T.Text, Maybe T.Text) -> RenderBlock
    encodeBlock b (fullText', shortText') = RenderBlock fullText' shortText' (b^.blockName)

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


runBarServer :: BarIO () -> MainOptions -> IO ()
runBarServer defaultBarConfig options = runBarHost barServer (swayBarInput options)
  where
    barServer :: Consumer [BlockOutput] BarIO ()
    barServer = do
      -- Load blocks
      lift $ do
        when (indicator options) $ addBlock renderIndicator
        defaultBarConfig

      -- Create control socket
      commandChan <- liftIO createCommandChan
      controlSocketAsync <- liftIO $ listenUnixSocketAsync options commandChan
      liftIO $ link controlSocketAsync

      bar <- askBar

      -- Update bar on control socket messages
      socketUpdateAsync <- liftIO $ async $ forever $ do
        -- command <- atomically $ readTChan commandChan
        void $ error "TODO"
        updateBar' bar
      liftIO $ link socketUpdateAsync

      swayBarOutput options

createCommandChan :: IO CommandChan
createCommandChan = newTChanIO

-- |Entry point.
runQBar :: BarIO () -> MainOptions -> IO ()
runQBar barConfiguration options@MainOptions{barCommand} = runCommand barCommand
  where
    runCommand BarServer = runBarServer barConfiguration options
    runCommand DefaultTheme = sendIpc options $ SetTheme "default"
    runCommand RainbowTheme = sendIpc options $ SetTheme "rainbow"
