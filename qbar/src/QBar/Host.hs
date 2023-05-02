module QBar.Host (
  HostHandle(..),
  attachBarOutput,
  eventDispatcher,
  filterDuplicates,
  installSignalHandlers,
  requestBarUpdateHandler,
  runBarHost',
  runBarHost,
  runBlocks,
) where

import QBar.BlockOutput
import QBar.Core
import QBar.Prelude
import QBar.Time
import QBar.Utils

import Control.Concurrent (forkIO, forkFinally, threadDelay)
import Control.Concurrent.Async (async, wait, waitAny)
import Control.Concurrent.Event qualified as Event
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Exception (SomeException, catch, fromException)
import Control.Lens hiding (each, (.=))
import Control.Monad.STM (atomically)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text.Lazy qualified as T
import Pipes
import Pipes.Concurrent (spawn, unbounded, toOutput, fromInput)
import System.Exit (ExitCode, exitWith)
import System.IO (stderr, hPutStrLn)
import System.Posix.Signals (Handler(..), sigCONT, installHandler)

data HostHandle = HostHandle {
  barUpdateEvent :: BarUpdateEvent,
  barUpdatedEvent :: Event.Event,
  followupEventWaitTimeMVar :: MVar Int,
  newBlockChan :: TChan BlockCache,
  eventHandlerListIORef :: IORef [(T.Text, BlockEventHandler)]
}

installSignalHandlers :: Bar -> IO ()
installSignalHandlers bar = void $ installHandler sigCONT (Catch sigContAction) Nothing
  where
    sigContAction :: IO ()
    sigContAction = do
      hPutStrLn stderr "SIGCONT received"
      updateBarDefault' bar


eventDispatcher :: Bar -> IORef [(T.Text, BlockEventHandler)] -> Consumer BlockEvent IO ()
eventDispatcher bar eventHandlerListIORef = eventDispatcher'
  where
    eventDispatcher' :: Consumer BlockEvent IO ()
    eventDispatcher' = do
      blockEvent <- await
      eventHandlerList <- liftIO $ readIORef eventHandlerListIORef
      let maybeEventHandler = getEventHandler eventHandlerList blockEvent
      case maybeEventHandler of
        Just eventHandler -> liftIO . void . forkIO $ catch (runBarIO bar $ eventHandler blockEvent) (\(e :: SomeException) -> hPutStrLn stderr $ "event handler failed: " <> show e)
        Nothing -> return ()
      eventDispatcher'
    getEventHandler :: [(T.Text, BlockEventHandler)] -> BlockEvent -> Maybe BlockEventHandler
    getEventHandler eventHandlerList blockEvent = lookup (name blockEvent) eventHandlerList


runBlocks :: Bar -> HostHandle -> Producer [BlockOutput] IO ()
runBlocks bar HostHandle{barUpdateEvent, barUpdatedEvent, followupEventWaitTimeMVar, newBlockChan, eventHandlerListIORef} = runBlocks' []
  where
    runBlocks' :: [BlockCache] -> Producer [BlockOutput] IO ()
    runBlocks' blocks = do
        -- Wait for an update request
      liftIO $ Event.wait barUpdateEvent

      -- Get current value and reset to default value
      followupEventWaitTime' <- liftIO $ swapMVar followupEventWaitTimeMVar followupEventWaitTimeDefault

      -- Wait for a moment (determined by block update reason) after the first event to catch (almost-)simultaneous block updates
      when (followupEventWaitTime' > 0) $ liftIO $ threadDelay followupEventWaitTime'
      liftIO $ Event.clear barUpdateEvent

      blocks' <- runBarIO bar $ addNewBlocks blocks

      (blockStates, blocks'') <- lift $ runBarIO bar $ getBlockStates blocks'

      -- Pass blocks to output
      yield $ map fst $ catMaybes blockStates

      -- Register new event handlers immediately after rendering
      liftIO $ updateEventHandlers blockStates

      liftIO $ Event.signal barUpdatedEvent

      -- Wait for 20ms after rendering a line to limit cpu load of rapid events
      liftIO $ threadDelay 20000

      -- Loop
      runBlocks' blocks''

    addNewBlocks :: [BlockCache] -> BarIO [BlockCache]
    addNewBlocks blocks = do
      maybeNewBlock <- liftIO $ atomically $ tryReadTChan newBlockChan
      case maybeNewBlock of
        Nothing -> return blocks
        Just newBlock -> addNewBlocks (newBlock:blocks)

    getBlockStates :: [BlockCache] -> BarIO ([BlockState], [BlockCache])
    getBlockStates caches = do
      (blockStates, newCaches) <- unzip . catMaybes <$> mapM readCache caches
      return (concat blockStates, newCaches)
      where
        readCache :: BlockCache -> BarIO (Maybe ([BlockState], BlockCache))
        readCache producer = do
          next' <- next producer
          return $ case next' of
            Left _ -> Nothing
            Right (blockStates, newProducer) -> Just (blockStates, newProducer)

    updateEventHandlers :: [BlockState] -> IO ()
    updateEventHandlers blockStates =
      writeIORef eventHandlerListIORef eventHandlerList
      where
        eventHandlerList :: [(T.Text, BlockEventHandler)]
        eventHandlerList = mapMaybe getEventHandler blockStates

        getEventHandler :: BlockState -> Maybe (T.Text, BlockEventHandler)
        getEventHandler Nothing = Nothing
        getEventHandler (Just (_, Nothing)) = Nothing
        getEventHandler (Just (blockOutput, Just eventHandler)) = do
          blockName' <- blockOutput ^. blockName
          return (blockName', eventHandler)


filterDuplicates :: (Monad m, Eq a) => Pipe a a m r
filterDuplicates = do
  value <- await
  yield value
  filterDuplicates' value
  where
    filterDuplicates' :: (Monad m, Eq a) => a -> Pipe a a m r
    filterDuplicates' lastValue = do
      value <- await
      when (lastValue /= value) $ yield value
      filterDuplicates' value


followupEventWaitTime :: BlockUpdateReason -> Int
followupEventWaitTime DefaultUpdate = 10000
followupEventWaitTime PollUpdate = 50000
-- 'followupEventWaitTime' for 'EventUpdate' has to be zero, or blocks would be blocked blocked for this time when sending a 'UserUpdate'.
followupEventWaitTime EventUpdate = 0

followupEventWaitTimeDefault :: Int
followupEventWaitTimeDefault = followupEventWaitTime PollUpdate

requestBarUpdateHandler :: HostHandle -> BlockUpdateReason -> IO ()
requestBarUpdateHandler HostHandle{barUpdateEvent, barUpdatedEvent, followupEventWaitTimeMVar} blockUpdateReason = do
  -- Configure followup event wait time
  modifyMVar_ followupEventWaitTimeMVar $ \current -> return $ min current $ followupEventWaitTime blockUpdateReason
  signalHost blockUpdateReason
  where
    signalHost :: BlockUpdateReason -> IO ()
    signalHost EventUpdate = do
      -- Start waiting before triggering the event cannot be missed
      task <- async $ Event.wait barUpdatedEvent
      Event.set barUpdateEvent
      -- Wait until the bar is updated. This happens almost immediately, but this ensures the block won't immediately override user feedback.
      wait task
    signalHost _ = Event.set barUpdateEvent


attachBarOutput :: (Consumer [BlockOutput] IO (), Producer BlockEvent IO ()) -> BarIO ()
attachBarOutput (blockOutputConsumer, blockEventProducer) = do
  Bar{attachBarOutputInternal} <- askBar
  liftIO $ attachBarOutputInternal (blockOutputConsumer, blockEventProducer)


runBarHost :: BarIO (Consumer [BlockOutput] IO (), Producer BlockEvent IO ()) -> BarIO () -> IO ()
runBarHost createHost loadBlocks = runBarHost' $ loadBlocks >> createHost >>= attachBarOutput


runBarHost' :: BarIO () -> IO ()
runBarHost' initializeBarAction = do
  -- Create an event used request bar updates
  barUpdateEvent <- Event.newSet
  -- Create an event that is signaled after bar updates
  barUpdatedEvent <- Event.new
  followupEventWaitTimeMVar <- newMVar 0

  -- Create channel to send new block producers to render loop
  newBlockChan <- newTChanIO

  barSleepScheduler <- createSleepScheduler

  -- Create IORef for event handlers
  eventHandlerListIORef <- newIORef []

  exitCodeMVar <- newEmptyMVar

  let hostHandle = HostHandle {
    barUpdateEvent,
    barUpdatedEvent,
    followupEventWaitTimeMVar,
    newBlockChan,
    eventHandlerListIORef
  }

  (eventOutput, eventInput) <- spawn unbounded

  -- Create cache for block outputs
  (cacheConsumer, cacheProducer) <- mkBroadcastCacheP []

  -- Important: both monads (output producer / event consumer) will be forked whenever a new output connects!
  let attachBarOutputInternal = attachBarOutputImpl exitCodeMVar cacheProducer (toOutput eventOutput)


  let requestBarUpdate = requestBarUpdateHandler hostHandle

  let bar = Bar {requestBarUpdate, newBlockChan, barSleepScheduler, attachBarOutputInternal}

  -- Install signal handler for SIGCONT
  installSignalHandlers bar

  -- Load blocks and initialize output handlers
  runBarIO bar initializeBarAction

  -- Run blocks and send filtered output to connected clients
  blockTask <- async $ runEffect $ runBlocks bar hostHandle >-> filterDuplicates >-> cacheConsumer
  -- Dispatch incoming events to blocks
  eventTask <- async $ runEffect $ fromInput eventInput >-> eventDispatcher bar eventHandlerListIORef

  exitTask <- async $ takeMVar exitCodeMVar >>= exitWith


  void $ waitAny [blockTask, eventTask, exitTask]

  where
    attachBarOutputImpl :: MVar ExitCode -> Producer [BlockOutput] IO () -> Consumer BlockEvent IO () -> (Consumer [BlockOutput] IO (), Producer BlockEvent IO ()) -> IO ()
    attachBarOutputImpl exitMVar blockOutputProducer eventConsumer (barOutputConsumer, barEventProducer) = do

      let
        handleBarEventInput :: IO ()
        handleBarEventInput = liftIO $ runEffect $ barEventProducer >-> eventConsumer
      liftIO $ void $ forkFinally handleBarEventInput $ handleOnExitCodeException (\result -> hPutStrLn stderr $ "An event input handler failed: " <> show result)

      let
        handleBarOutput :: IO ()
        handleBarOutput = liftIO $ runEffect $ blockOutputProducer >-> filterDuplicates >-> barOutputConsumer
      liftIO $ void $ forkFinally handleBarOutput $ handleOnExitCodeException (\result -> hPutStrLn stderr $ "A bar output handler failed: " <> show result)

      where
        -- Calls the next handler unless the exception is an ExitCode.
        handleOnExitCodeException :: (Either SomeException () -> IO ()) -> Either SomeException () -> IO ()
        handleOnExitCodeException nextHandler x@(Left ex) = case fromException ex of
          Just exitCode -> do
            hPutStrLn stderr "Exiting"
            putMVar exitMVar exitCode
          Nothing -> nextHandler x
        handleOnExitCodeException nextHandler x = nextHandler x
