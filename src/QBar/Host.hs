{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}

module QBar.Host where

import QBar.BlockOutput
import QBar.Core

import Control.Concurrent (forkIO, forkFinally, threadDelay)
import Control.Concurrent.Event as Event
import Control.Concurrent.STM.TChan (TChan, newTChanIO, tryReadTChan)
import Control.Exception (SomeException, catch)
import Control.Lens hiding (each, (.=))
import Control.Monad (when)
import Control.Monad.STM (atomically)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Text.Lazy as T
import Pipes
import System.IO (stderr, hPutStrLn)
import System.Posix.Signals

data HostHandle = HostHandle {
  barUpdateEvent :: BarUpdateEvent,
  newBlockChan :: TChan BlockCache,
  eventHandlerListIORef :: IORef [(T.Text, BlockEventHandler)]
}

installSignalHandlers :: Bar -> IO ()
installSignalHandlers bar = void $ installHandler sigCONT (Catch sigContAction) Nothing
  where
    sigContAction :: IO ()
    sigContAction = do
      hPutStrLn stderr "SIGCONT received"
      updateBar' bar

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
runBlocks bar HostHandle{barUpdateEvent, newBlockChan, eventHandlerListIORef} = runBlocks' []
  where
    runBlocks' :: [BlockCache] -> Producer [BlockOutput] IO ()
    runBlocks' blocks = do
      liftIO $ do
        -- Wait for an update request
        Event.wait barUpdateEvent

        -- Wait for 10ms after first events to catch (almost-)simultaneous event updates
        threadDelay 10000
        Event.clear barUpdateEvent

      blocks' <- liftIO $ runBarIO bar $ addNewBlocks blocks

      (blockStates, blocks'') <- liftIO $ runBarIO bar $ getBlockStates blocks'

      -- Pass blocks to output
      yield $ map fst $ catMaybes blockStates

      -- Register new event handlers immediately after rendering
      liftIO $ updateEventHandlers blockStates

      -- Wait for 90ms after rendering a line to limit cpu load of rapid events
      liftIO $ threadDelay 90000

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


runBarHost :: Consumer [BlockOutput] IO ()
  -> Producer BlockEvent IO ()
  -> BarIO ()
  -> IO ()
runBarHost host barEventProducer loadBlocks = do
  -- Create an event used to signal bar updates
  barUpdateEvent <- Event.newSet
  let requestBarUpdate = Event.set barUpdateEvent

  -- Create channel to send new block producers to render loop
  newBlockChan <- newTChanIO

  let bar = Bar { requestBarUpdate, newBlockChan }

  -- Install signal handler for SIGCONT
  installSignalHandlers bar

  -- Create IORef for event handlers
  eventHandlerListIORef <- newIORef []

  let hostHandle = HostHandle {
    barUpdateEvent,
    newBlockChan,
    eventHandlerListIORef
  }

  runBarIO bar loadBlocks

  let handleStdin = liftIO $ runEffect $ barEventProducer >-> eventDispatcher bar eventHandlerListIORef
  -- Fork stdin handler
  void $ forkFinally (runBarIO bar handleStdin) (\result -> hPutStrLn stderr $ "handleStdin failed: " <> show result)

  -- Run bar host
  runEffect $ runBlocks bar hostHandle >-> filterDuplicates >-> host

