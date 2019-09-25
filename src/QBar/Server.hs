{-# LANGUAGE OverloadedStrings #-}

module QBar.Server where

import QBar.Blocks
import QBar.Core
import QBar.Cli
import QBar.ControlSocket
import QBar.Filter

import Control.Monad (forever, when, unless)
import Control.Monad.STM (atomically)
import Control.Concurrent (threadDelay, forkFinally)
import Control.Concurrent.Async
import Control.Concurrent.Event as Event
import Control.Concurrent.STM.TChan (newTChanIO, readTChan)
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
import System.IO (stdin, stdout, stderr, hFlush, hPutStrLn)
import System.Posix.Signals

data Handle = Handle {
  handleActionList :: IORef [(T.Text, IO ())],
  handleActiveFilter :: IORef Filter
}

renderIndicator :: BlockProducer
renderIndicator = forever $ each $ map createBlock ["/", "-", "\\", "|"]

runBlock :: BlockProducer -> IO (Maybe (Block, BlockProducer))
runBlock producer = do
  next' <- next producer
  return $ case next' of
    Left _ -> Nothing
    Right (block, newProducer) -> Just (block, newProducer)

runBlocks :: [BlockProducer] -> IO ([Block], [BlockProducer])
runBlocks blockProducers = unzip . catMaybes <$> mapM runBlock blockProducers

renderLoop :: MainOptions -> Handle -> BarUpdateEvent -> BS.ByteString -> [BlockProducer] -> IO ()
renderLoop options handle@Handle{handleActiveFilter} barUpdateEvent = renderLoop'
  where
    renderLoop' :: BS.ByteString -> [BlockProducer] -> IO ()
    renderLoop' previousBarOutput blockProducers = do
      blockFilter <- readIORef handleActiveFilter

      -- Wait for an event (unless the filter is animated)
      unless (isAnimatedFilter blockFilter) $ Event.wait barUpdateEvent

      -- Wait for 10ms after first events to catch (almost-)simultaneous event updates
      threadDelay 10000
      Event.clear barUpdateEvent

      (blocks, blockProducers') <- runBlocks blockProducers

      currentBarOutput <- renderLine options handle blockFilter blocks previousBarOutput

      -- Wait for 100ms after rendering a line to limit cpu load of rapid events
      threadDelay 100000

      renderLoop' currentBarOutput blockProducers'

renderLine :: MainOptions -> Handle -> Filter -> [Block] -> BS.ByteString -> IO BS.ByteString
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
    clickActionList :: [(T.Text, IO ())]
    clickActionList = mapMaybe getClickAction blocks
    getClickAction :: Block -> Maybe (T.Text, IO ())
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

handleStdin :: MainOptions -> IORef [(T.Text, IO ())] -> IO ()
handleStdin options actionListIORef = forever $ do
  line <- BSSC8.hGetLine stdin

  unless (line == "[") $ do
    -- Echo input to stderr when verbose flag is set
    when (verbose options) $ do
      BSSC8.hPutStrLn stderr line
      hFlush stderr

    clickActionList <- readIORef actionListIORef
    let maybeParsedClick = decode $ removeComma $ BS.fromStrict line
    let clickAction' = getClickAction clickActionList maybeParsedClick
    async (fromMaybe (return ()) clickAction') >>= link

  where
    getClickAction :: [(T.Text, IO ())] -> Maybe Click -> Maybe (IO ())
    getClickAction clickActionList maybeParsedClick = do
      parsedClick <- maybeParsedClick
      let blockName = name parsedClick
      lookup blockName clickActionList
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

runI3BarConfiguration :: (BarUpdateChannel -> IO [BlockProducer]) -> MainOptions -> IO ()
runI3BarConfiguration generateBarConfig options = do
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

  date <- dateBlock
  let initialBlocks = [date]

  -- Attach spinner indicator when verbose flag is set
  let initialBlocks' = if verbose options then initialBlocks <> [createBlock "*"] else initialBlocks

  -- Render initial time block so the bar is not empty after startup
  initialOutput <- renderLine options handle initialBlockFilter initialBlocks' ""

  -- Create and initialzie blocks
  (barUpdateChannel, barUpdateEvent) <- createBarUpdateChannel
  blockProducers <- generateBarConfig barUpdateChannel

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

  -- Attach spinner indicator when verbose flag is set
  let blockProducers' = if verbose options then blockProducers <> [renderIndicator] else blockProducers

  renderLoop options handle barUpdateEvent initialOutput blockProducers'

createCommandChan :: IO CommandChan
createCommandChan = newTChanIO