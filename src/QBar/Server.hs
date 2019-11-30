{-# LANGUAGE OverloadedStrings #-}

module QBar.Server where

import QBar.Blocks
import QBar.Core
import QBar.Cli
import QBar.ControlSocket
import QBar.Filter

import Control.Monad (forever, when, unless, forM_)
import Control.Monad.STM (atomically)
import Control.Concurrent (threadDelay, forkFinally)
import Control.Concurrent.Async
import Control.Concurrent.Event as Event
import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan, writeTChan, tryReadTChan)
import Data.Aeson (encode, decode)
import Data.ByteString.Lazy (hPut)
import qualified Data.ByteString.Char8 as BSSC8
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.IORef
import Data.Maybe (isJust, fromJust, fromMaybe, catMaybes, mapMaybe)
import qualified Data.Text.Lazy as T
import Data.Time.Clock.POSIX
import Pipes
import Pipes.Prelude (toListM)
import System.IO (stdin, stdout, stderr, hFlush, hPutStrLn)
import System.Posix.Signals

data Handle = Handle {
  handleActionList :: IORef [(T.Text, Click -> IO ())],
  handleActiveFilter :: IORef Filter
}

renderIndicator :: CachedBlock
-- Using 'cachedBlock' is a hack to actually get the block to update on every bar update (by doing this it will not get a cache later in the pipeline).
renderIndicator = forever $ each $ map createBlock ["/", "-", "\\", "|"]

runBlock :: CachedBlockProducer -> IO (Maybe (BlockOutput, CachedBlockProducer))
runBlock producer = do
  next' <- next producer
  return $ case next' of
    Left _ -> Nothing
    Right (block, newProducer) -> Just (block, newProducer)

runBlocks :: [CachedBlockProducer] -> IO ([BlockOutput], [CachedBlockProducer])
runBlocks blockProducers = unzip . catMaybes <$> mapM runBlock blockProducers

renderLoop :: MainOptions -> Handle -> BarUpdateEvent -> BS.ByteString -> TChan CachedBlock -> IO ()
renderLoop options handle@Handle{handleActiveFilter} barUpdateEvent previousBarOutput newBlockChan = renderLoop' previousBarOutput []
  where
    addNewBlockProducers :: [CachedBlock] -> IO [CachedBlock]
    addNewBlockProducers blockProducers = do
      maybeNewBlock <- atomically $ tryReadTChan newBlockChan
      case maybeNewBlock of
        Nothing -> return blockProducers
        Just newBlock -> addNewBlockProducers (newBlock:blockProducers)
    renderLoop' :: BS.ByteString -> [CachedBlock] -> IO ()
    renderLoop' previousBarOutput' blockProducers = do
      blockFilter <- readIORef handleActiveFilter

      -- Wait for an event (unless the filter is animated)
      unless (isAnimatedFilter blockFilter) $ Event.wait barUpdateEvent

      -- Wait for 10ms after first events to catch (almost-)simultaneous event updates
      threadDelay 10000
      Event.clear barUpdateEvent

      blockProducers' <- addNewBlockProducers blockProducers

      (blocks, blockProducers'') <- runBlocks blockProducers'

      currentBarOutput <- renderLine options handle blockFilter blocks previousBarOutput'

      -- Wait for 100ms after rendering a line to limit cpu load of rapid events
      threadDelay 100000

      renderLoop' currentBarOutput blockProducers''

renderLine :: MainOptions -> Handle -> Filter -> [BlockOutput] -> BS.ByteString -> IO BS.ByteString
renderLine MainOptions{verbose} Handle{handleActionList} blockFilter blocks previousEncodedOutput = do
  time <- fromRational . toRational <$> getPOSIXTime
  let filteredBlocks = applyFilter blockFilter time blocks
  let encodedOutput = encode $ map values filteredBlocks
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
    clickActionList :: [(T.Text, Click -> IO ())]
    clickActionList = mapMaybe getClickAction blocks
    getClickAction :: BlockOutput -> Maybe (T.Text, Click -> IO ())
    getClickAction block = if hasBlockName && hasClickAction then Just (fromJust maybeBlockName, fromJust maybeClickAction) else Nothing
      where
        maybeBlockName = getBlockName block
        hasBlockName = isJust maybeBlockName
        maybeClickAction = clickAction block
        hasClickAction = isJust maybeClickAction

createBarUpdateChannel :: IO (BarUpdateChannel, BarUpdateEvent)
createBarUpdateChannel = do
  event <- Event.newSet
  return (BarUpdateChannel $ Event.set event, event)

handleStdin :: MainOptions -> IORef [(T.Text, Click -> IO ())] -> IO ()
handleStdin options actionListIORef = forever $ do
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
        let clickAction' = getClickAction clickActionList click
        async ((fromMaybe discard clickAction') click) >>= link
      Nothing -> return ()

  where
    getClickAction :: [(T.Text, Click -> IO ())] -> Click -> Maybe (Click -> IO ())
    getClickAction clickActionList click = lookup (name click) clickActionList
    removeComma :: C8.ByteString -> C8.ByteString
    removeComma line
      | C8.head line == ',' = C8.tail line
      | C8.last line == ',' = C8.init line
      | otherwise = line

installSignalHandlers :: BarUpdateChannel -> IO ()
installSignalHandlers barUpdateChannel = void $ installHandler sigCONT (Catch sigContAction) Nothing
  where
    sigContAction :: IO ()
    sigContAction = do
      hPutStrLn stderr "SIGCONT received"
      updateBar barUpdateChannel

runBarConfiguration :: (BarUpdateChannel -> Producer CachedBlock IO ()) -> MainOptions -> IO ()
runBarConfiguration generateBarConfig options = do
  -- Create IORef for mouse click callbacks
  actionList <- newIORef []
  --link =<< async (handleStdin options actionList)
  void $ forkFinally (handleStdin options actionList) (\result -> hPutStrLn stderr $ "handleStdin failed: " <> show result)

  -- Create IORef to contain the active filter
  let initialBlockFilter = StaticFilter None
  activeFilter <- newIORef initialBlockFilter

  let handle = Handle {
    handleActionList = actionList,
    handleActiveFilter = activeFilter
  }

  putStrLn "{\"version\":1,\"click_events\":true}"
  putStrLn "["

  date <- dateBlockOutput
  let initialBlocks = [date]

  -- Attach spinner indicator when verbose flag is set
  let initialBlocks' = if indicator options then initialBlocks <> [createBlock "*"] else initialBlocks

  -- Render initial time block so the bar is not empty after startup
  initialOutput <- renderLine options handle initialBlockFilter initialBlocks' ""

  -- Create and initialzie blocks
  (barUpdateChannel, barUpdateEvent) <- createBarUpdateChannel
  blockProducers <- toListM $ generateBarConfig barUpdateChannel

  -- Attach spinner indicator when verbose flag is set
  let blockProducers' = if indicator options then  (renderIndicator:blockProducers) else blockProducers

  -- Create channel to send new block producers to render loop
  newBlockProducers <- newTChanIO

  -- Send initial block producers to render loop
  forM_ blockProducers' $ \ bp -> atomically $ writeTChan newBlockProducers bp

  -- Install signal handler for SIGCONT
  installSignalHandlers barUpdateChannel

  -- Create control socket
  commandChan <- createCommandChan
  controlSocketAsync <- listenUnixSocketAsync options commandChan
  link controlSocketAsync

  -- Update bar on control socket messages
  socketUpdateAsync <- async $ forever $ do
    command <- atomically $ readTChan commandChan
    case command of
      SetFilter blockFilter -> atomicWriteIORef activeFilter blockFilter
    updateBar barUpdateChannel
  link socketUpdateAsync

  renderLoop options handle barUpdateEvent initialOutput newBlockProducers

createCommandChan :: IO CommandChan
createCommandChan = newTChanIO