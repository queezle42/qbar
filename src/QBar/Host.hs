{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

module QBar.Host where

import QBar.BlockOutput
import QBar.Core
import QBar.Time

import Control.Concurrent (ThreadId, forkIO, forkFinally, threadDelay, myThreadId, throwTo)
import Control.Concurrent.Async (async, wait)
import qualified Control.Concurrent.Event as Event
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_, swapMVar)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, tryReadTChan)
import Control.Exception (SomeException, catch)
import Control.Lens hiding (each, (.=))
import Control.Monad.STM (atomically)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Text.Lazy as T
import Pipes
import System.Exit
import System.IO (stderr, hPutStrLn)
import System.Posix.Process (createSession)
import System.Posix.Signals (Handler(..), sigTERM, sigCONT, signalProcessGroup, installHandler)
import System.Posix.Types (ProcessGroupID)

data HostHandle = HostHandle {
  barUpdateEvent :: BarUpdateEvent,
  barUpdatedEvent :: Event.Event,
  followupEventWaitTimeMVar :: MVar Int,
  newBlockChan :: TChan BlockCache,
  eventHandlerListIORef :: IORef [(T.Text, BlockEventHandler)]
}

installSignalHandlers :: Bar -> IO ()
installSignalHandlers bar = do
  mainThread <- myThreadId
  processGroup <- createSession
  void $ installHandler sigCONT (Catch sigContAction) Nothing
  void $ installHandler sigTERM (Catch $ sigTermAction mainThread processGroup) Nothing
  where
    sigContAction :: IO ()
    sigContAction = do
      hPutStrLn stderr "SIGCONT received"
      updateBarDefault' bar
    sigTermAction :: ThreadId -> ProcessGroupID -> IO ()
    sigTermAction mainThread processGroup = do
      void $ installHandler sigTERM Ignore Nothing
      hPutStrLn stderr "SIGTERM received"
      hPutStrLn stderr "Killing process group"
      signalProcessGroup sigTERM processGroup
      throwTo mainThread $ ExitFailure 143


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
        eventHandlerList = mapMaybe getEventHandler $ blockStates

        getEventHandler :: BlockState -> Maybe (T.Text, BlockEventHandler)
        getEventHandler Nothing = Nothing
        getEventHandler (Just (_, Nothing)) = Nothing
        getEventHandler (Just (blockOutput, Just eventHandler)) = do
          blockName' <- blockOutput^.blockName
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
followupEventWaitTime PullUpdate = 50000
-- 'followupEventWaitTime' for 'UserUpdate' has to be zero, or blocks would be blocked blocked for this time when sending a 'UserUpdate'.
followupEventWaitTime UserUpdate = 0

followupEventWaitTimeDefault :: Int
followupEventWaitTimeDefault = followupEventWaitTime PullUpdate

requestBarUpdateHandler :: HostHandle -> BlockUpdateReason -> IO ()
requestBarUpdateHandler HostHandle{barUpdateEvent, barUpdatedEvent, followupEventWaitTimeMVar} blockUpdateReason = do
  -- Configure followup event wait time
  modifyMVar_ followupEventWaitTimeMVar $ \current -> return $ min current $ followupEventWaitTime blockUpdateReason
  signalHost blockUpdateReason
  where
    signalHost :: BlockUpdateReason -> IO ()
    signalHost UserUpdate = do
      -- Start waiting before triggering the event cannot be missed
      task <- async $ Event.wait barUpdatedEvent
      Event.set barUpdateEvent
      -- Wait until the bar is updated. This happens almost immediately, but this ensures the block won't immediately override user feedback.
      wait task
    signalHost _ = Event.set barUpdateEvent


runBarHost :: BarIO (Consumer [BlockOutput] IO (), Producer BlockEvent IO ()) -> BarIO () -> IO ()
runBarHost createHost loadBlocks = do
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

  let hostHandle = HostHandle {
    barUpdateEvent,
    barUpdatedEvent,
    followupEventWaitTimeMVar,
    newBlockChan,
    eventHandlerListIORef
  }

  let requestBarUpdate = requestBarUpdateHandler hostHandle

  let bar = Bar {requestBarUpdate, newBlockChan, barSleepScheduler}

  -- Install signal handler for SIGCONT
  installSignalHandlers bar

  runBarIO bar loadBlocks

  (host, barEventProducer) <- runBarIO bar createHost

  let handleStdin = liftIO $ runEffect $ barEventProducer >-> eventDispatcher bar eventHandlerListIORef
  -- Fork stdin handler
  void $ forkFinally (runBarIO bar handleStdin) (\result -> hPutStrLn stderr $ "handleStdin failed: " <> show result)

  -- Run bar host
  runEffect $ runBlocks bar hostHandle >-> filterDuplicates >-> host
