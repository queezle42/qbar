module QBar.Server where

import QBar.Blocks
import QBar.Core
import QBar.Cli
import QBar.ControlSocket
import QBar.Filter
import QBar.BlockText
import QBar.Themes

import Control.Monad (forever, when, unless)
import Control.Monad.STM (atomically)
import Control.Concurrent (threadDelay, forkFinally)
import Control.Concurrent.Async
import Control.Concurrent.Event as Event
import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan, tryReadTChan)
import Data.Aeson (encode, decode, ToJSON, toJSON, object, (.=))
import Data.ByteString.Lazy (hPut)
import qualified Data.ByteString.Char8 as BSSC8
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.IORef
import Data.Maybe (catMaybes, mapMaybe, fromMaybe)
import qualified Data.Text.Lazy as T
import Data.Time.Clock.POSIX
import Pipes
import System.IO (stdin, stdout, stderr, hFlush, hPutStrLn)
import System.Posix.Signals
import Control.Lens hiding (each, (.=))

data Handle = Handle {
  handleActionList :: IORef [(T.Text, Click -> BarIO ())],
  handleActiveFilter :: IORef Filter
}

renderIndicator :: CachedBlock
-- Using 'cachedBlock' is a hack to actually get the block to update on every bar update (by doing this it will not get a cache later in the pipeline).
renderIndicator = forever $ each $ map (Just . createBlock . normalText) ["/", "-", "\\", "|"]

runBlock :: CachedBlock -> BarIO (Maybe (Maybe BlockOutput, CachedBlock))
runBlock producer = do
  next' <- next producer
  return $ case next' of
    Left _ -> Nothing
    Right (block, newProducer) -> Just (block, newProducer)

runBlocks :: [CachedBlock] -> BarIO ([Maybe BlockOutput], [CachedBlock])
runBlocks block = unzip . catMaybes <$> mapM runBlock block

data RenderBlock = RenderBlock T.Text (Maybe T.Text) (Maybe T.Text)
  deriving(Show)
instance ToJSON RenderBlock where
  toJSON (RenderBlock fullText' shortText' blockName') = object $
    fullText'' <> shortText'' <> blockName'' <> pango''
    where
      fullText'' = [ "full_text" .= fullText' ]
      shortText'' = fromMaybe (\s -> ["short_text".=s]) mempty shortText'
      blockName'' = fromMaybe (\s -> ["name".=s]) mempty blockName'
      pango'' = [ "markup" .= ("pango" :: T.Text) ]



renderLoop :: MainOptions -> Handle -> BarUpdateEvent -> BS.ByteString -> TChan CachedBlock -> BarIO ()
renderLoop options handle@Handle{handleActiveFilter} barUpdateEvent previousBarOutput newBlockChan = renderLoop' previousBarOutput []
  where
    addNewBlocks :: [CachedBlock] -> BarIO [CachedBlock]
    addNewBlocks blocks = do
      maybeNewBlock <- liftIO $ atomically $ tryReadTChan newBlockChan
      case maybeNewBlock of
        Nothing -> return blocks
        Just newBlock -> addNewBlocks (newBlock:blocks)
    renderLoop' :: BS.ByteString -> [CachedBlock] -> BarIO ()
    renderLoop' previousBarOutput' blocks = do
      blockFilter <- liftIO $ readIORef handleActiveFilter

      -- Wait for an event (unless the filter is animated)
      unless (isAnimatedFilter blockFilter) $ liftIO $ Event.wait barUpdateEvent

      -- Wait for 10ms after first events to catch (almost-)simultaneous event updates
      liftIO $ threadDelay 10000
      liftIO $ Event.clear barUpdateEvent

      blocks' <- addNewBlocks blocks

      (blockOutputs, blocks'') <- runBlocks blocks'

      currentBarOutput <- liftIO $ renderLine options handle blockFilter blockOutputs previousBarOutput'

      -- Wait for 100ms after rendering a line to limit cpu load of rapid events
      liftIO $ threadDelay 100000

      renderLoop' currentBarOutput blocks''

renderLine :: MainOptions -> Handle -> Filter -> [Maybe BlockOutput] -> BS.ByteString -> IO BS.ByteString
renderLine MainOptions{verbose} Handle{handleActionList} blockFilter blocks' previousEncodedOutput = do
  time <- fromRational . toRational <$> getPOSIXTime
  let blocks = catMaybes blocks'
  let filteredBlocks = applyFilter blockFilter time blocks
  -- let encodedOutput = encode $ map values filteredBlocks
  let encodedOutput = encodeOutput filteredBlocks
  let changed = previousEncodedOutput /= encodedOutput
  when changed $ do
    hPut stdout encodedOutput
    putStrLn ","
    hFlush stdout
    -- Echo output to stderr when verbose flag is set
    when verbose $ do
      hPut stderr encodedOutput
      hPut stderr "\n"
      hFlush stderr

  when verbose $ unless changed $ hPutStrLn stderr "Output unchanged"

  -- Register click handlers regardless of bar changes, because we cannot easily check if any handler has changed
  writeIORef handleActionList clickActionList

  return encodedOutput
  where
    theme :: Theme
    theme = defaultTheme
    encodeOutput :: [BlockOutput] -> BS.ByteString
    encodeOutput bs = encode $ zipWith encodeBlock bs $ theme bs
    encodeBlock :: BlockOutput -> (T.Text, Maybe T.Text) -> RenderBlock
    encodeBlock b (fullText', shortText') = RenderBlock fullText' shortText' (b^.blockName)
    clickActionList :: [(T.Text, Click -> BarIO ())]
    clickActionList = mapMaybe getClickAction . catMaybes $ blocks'
    getClickAction :: BlockOutput -> Maybe (T.Text, Click -> BarIO ())
    getClickAction block = do
      blockName' <- block^.blockName
      clickAction' <- block^.clickAction
      return (blockName', clickAction')

createBarUpdateChannel :: IO (IO (), BarUpdateEvent)
createBarUpdateChannel = do
  event <- Event.newSet
  return (Event.set event, event)

handleStdin :: MainOptions -> IORef [(T.Text, Click -> BarIO ())] -> BarIO ()
handleStdin options actionListIORef = do
  bar <- askBar
  liftIO $ forever $ do
    line <- BSSC8.hGetLine stdin

    unless (line == "[") $ do
      -- Echo input to stderr when verbose flag is set
      when (verbose options) $ do
        BSSC8.hPutStrLn stderr line
        hFlush stderr

      let maybeClick = decode $ removeComma $ BS.fromStrict line
      case maybeClick of
        Just click -> do
          clickActionList <- readIORef actionListIORef
          let maybeClickAction = getClickAction clickActionList click
          case maybeClickAction of
            Just clickAction' -> async (runBarIO bar (clickAction' click)) >>= link
            Nothing -> return ()
        Nothing -> return ()

  where
    getClickAction :: [(T.Text, Click -> BarIO ())] -> Click -> Maybe (Click -> BarIO ())
    getClickAction clickActionList click = lookup (name click) clickActionList
    removeComma :: C8.ByteString -> C8.ByteString
    removeComma line
      | C8.head line == ',' = C8.tail line
      | C8.last line == ',' = C8.init line
      | otherwise = line

installSignalHandlers :: BarIO ()
installSignalHandlers = do
  bar <- askBar
  liftIO $ void $ installHandler sigCONT (Catch (sigContAction bar)) Nothing
  where
    sigContAction :: Bar -> IO ()
    sigContAction bar = do
      hPutStrLn stderr "SIGCONT received"
      updateBar' bar

renderInitialBlocks :: MainOptions -> Handle -> Filter -> IO C8.ByteString
renderInitialBlocks options handle blockFilter = do
  date <- dateBlockOutput
  let initialBlocks = [Just date]
  -- Attach spinner indicator when verbose flag is set
  let initialBlocks' = if indicator options then initialBlocks <> [Just . createBlock . normalText $ "*"] else initialBlocks
  -- Render initial time block so the bar is not empty after startup
  renderLine options handle blockFilter initialBlocks' ""


runBarConfiguration :: BarIO () -> MainOptions -> IO ()
runBarConfiguration defaultBarConfig options = do
  -- Create IORef to contain the active filter
  let initialBlockFilter = StaticFilter None
  activeFilter <- newIORef initialBlockFilter

  putStrLn "{\"version\":1,\"click_events\":true}"
  putStrLn "["

  (requestBarUpdate, barUpdateEvent) <- createBarUpdateChannel

  -- Create channel to send new block producers to render loop
  newBlockChan <- newTChanIO

  let bar = Bar { requestBarUpdate, newBlockChan }

  -- Create IORef for mouse click callbacks
  actionList <- newIORef []
  let handle = Handle {
    handleActionList = actionList,
    handleActiveFilter = activeFilter
  }

  initialOutput <- renderInitialBlocks options handle initialBlockFilter


  -- Fork stdin handler
  void $ forkFinally (runBarIO bar (handleStdin options actionList)) (\result -> hPutStrLn stderr $ "handleStdin failed: " <> show result)


  runBarIO bar loadBlocks

  -- Install signal handler for SIGCONT
  runBarIO bar installSignalHandlers

  -- Create control socket
  commandChan <- createCommandChan
  controlSocketAsync <- listenUnixSocketAsync options commandChan
  link controlSocketAsync

  -- Update bar on control socket messages
  socketUpdateAsync <- async $ forever $ do
    command <- atomically $ readTChan commandChan
    case command of
      SetFilter blockFilter -> atomicWriteIORef activeFilter blockFilter
      Block -> error "TODO"
    updateBar' bar
  link socketUpdateAsync

  runBarIO bar (renderLoop options handle barUpdateEvent initialOutput newBlockChan)
  where
    loadBlocks :: BarIO ()
    loadBlocks = do
      when (indicator options) $ addBlock renderIndicator

      defaultBarConfig


createCommandChan :: IO CommandChan
createCommandChan = newTChanIO

-- |Entry point.
runQBar :: BarIO () -> MainOptions -> IO ()
runQBar barConfiguration options@MainOptions{barCommand} = runCommand barCommand
  where
    runCommand BarServer = runBarConfiguration barConfiguration options
    runCommand NoFilter = sendIpc options $ SetFilter $ StaticFilter None
    runCommand RainbowFilter = sendIpc options $ SetFilter $ AnimatedFilter Rainbow
