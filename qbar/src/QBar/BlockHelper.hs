module QBar.BlockHelper (
  PollBlock',
  PollBlock,
  PollSignal,
  Signal(..),
  SignalBlock,
  SignalBlockConfiguration(..),
  respondBlockUpdate,
  respondEmptyBlockUpdate,
  runPollBlock',
  runPollBlock,
  runSignalBlock,
  runSignalBlockConfiguration,
  runSignalBlockFn',
  runSignalBlockFn,
  yieldBlockUpdate,
  yieldEmptyBlockUpdate,
) where

import QBar.BlockOutput
import QBar.Core
import QBar.Prelude
import QBar.Time

import Control.Concurrent.Async
import qualified Control.Concurrent.Event as Event
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Monad.Reader (ReaderT)
import Control.Lens
import Data.Either (isRight)
import Pipes
import Pipes.Concurrent
import Pipes.Core
import Pipes.Safe (bracket, runSafeT)

data Signal a = RegularSignal | UserSignal a | EventSignal BlockEvent
  deriving (Show, Eq)

type SignalBlock a = (Signal a -> Server (Signal a) (Maybe BlockOutput) BarIO ExitBlock)

-- |Block that 'respond's with an update whenever it receives a 'PollSignal'.
type PollBlock = Server PollSignal (Maybe BlockOutput) BarIO ExitBlock
type PollBlock' = Server PollSignal (Maybe BlockOutput) BarIO
data PollSignal = PollSignal

respondBlockUpdate :: BlockOutput -> Server' (Signal s) (Maybe BlockOutput) BarIO (Signal s)
respondBlockUpdate blockOutput = respond $ Just blockOutput

-- |Update a block by removing the current output
respondEmptyBlockUpdate :: Server' (Signal s) (Maybe BlockOutput) BarIO (Signal s)
respondEmptyBlockUpdate = respond Nothing


yieldBlockUpdate :: BlockOutput -> Server' PollSignal (Maybe BlockOutput) BarIO ()
yieldBlockUpdate blockOutput = void . respond $ Just blockOutput

-- |Update a block by removing the current output
yieldEmptyBlockUpdate :: Server' PollSignal (Maybe BlockOutput) BarIO ()
yieldEmptyBlockUpdate = void . respond $ Nothing


runSignalBlock :: forall a. Maybe Interval -> Maybe ((a -> IO ()) -> BarIO ()) -> SignalBlock a -> Block
runSignalBlock maybeInterval maybeSignalSourceThread signalBlock' = runSignalBlockConfiguration $ SignalBlockConfiguration {
  aquire = const $ return (),
  release = return,
  signalThread = const <$> maybeSignalSourceThread,
  signalBlock = const signalBlock',
  interval = maybeInterval
}


runSignalBlockFn :: forall a. Maybe Interval -> ((a -> IO ()) -> BarIO ()) -> ((a, Maybe BlockEvent) -> BarIO (Maybe BlockOutput)) -> Block
runSignalBlockFn maybeInterval signalSourceThread renderFn = runSignalBlock maybeInterval (Just signalSourceThread) signalBlock
  where
    signalBlock :: SignalBlock a
    signalBlock (UserSignal value) = signalBlock' value (UserSignal value)
    signalBlock _ = signalBlock =<< respondEmptyBlockUpdate
    signalBlock' :: a -> SignalBlock a
    signalBlock' state RegularSignal = signalBlock' state =<< respond =<< lift (renderFn (state, Nothing))
    signalBlock' _ (UserSignal value) = signalBlock' value =<< respond =<< lift (renderFn (value, Nothing))
    signalBlock' state (EventSignal event) = signalBlock' state =<< respond =<< lift (renderFn (state, Just event))

runSignalBlockFn' :: Maybe Interval -> (Maybe BlockEvent -> BarIO (Maybe BlockOutput)) -> Block
runSignalBlockFn' maybeInterval renderFn = runSignalBlockConfiguration $ SignalBlockConfiguration {
  aquire = const $ return (),
  release = return,
  signalThread = Nothing,
  signalBlock = const eventBlock,
  interval = maybeInterval
}
  where
    eventBlock :: SignalBlock a
    eventBlock (EventSignal event) = eventBlock =<< respond =<< lift (renderFn (Just event))
    eventBlock _ = eventBlock =<< respond =<< lift (renderFn Nothing)



data SignalBlockConfiguration c p = SignalBlockConfiguration {
  aquire :: (p -> IO ()) -> BarIO c,
  release :: c -> BarIO (),
  signalThread :: Maybe (c -> (p -> IO ()) -> BarIO ()),
  signalBlock :: c -> SignalBlock p,
  interval :: Maybe Interval
}

runSignalBlockConfiguration :: forall c p. SignalBlockConfiguration c p -> Block
runSignalBlockConfiguration SignalBlockConfiguration{aquire, release, signalThread, signalBlock, interval} = do
  -- Initialize
  signalChan <- liftIO newTChanIO
  signalEvent <- liftIO Event.new
  -- renderStateVar: (current BlockUpdate or Nothing when signal block terminated, invalidated)
  renderStateVar <- liftIO $ newTVarIO (Just (Nothing, PollUpdate), False)
  -- renderEvent: Signals an update to renderStateVar
  renderEvent <- liftIO Event.new

  runSignalBlockWithThreadInternal signalChan signalEvent renderStateVar renderEvent
  where
    runSignalBlockWithThreadInternal :: TChan (Signal p) -> Event.Event -> TVar (Maybe BlockUpdate, Bool) -> Event.Event -> Block
    runSignalBlockWithThreadInternal signalChan signalEvent renderStateVar renderEvent = do
      generatorTask <- barAsync $ bracket aquire' release' (\(context, _, _) -> runEffect $ void (signalBlock context +>> signalPipe))
      liftIO $ link generatorTask
      renderer
      where
        renderer :: Block
        renderer = do
          liftIO $ Event.wait renderEvent
          liftIO $ Event.clear renderEvent

          currentState <- liftIO (readTVarIO renderStateVar)
          renderer' currentState
          where
            renderer' :: (Maybe BlockUpdate, Bool) -> Block
            renderer' (Just (blockState, reason), invalidated) = do
              yield $ if invalidated then (invalidateBlockState blockState, reason) else (blockState, reason)
              renderer
            renderer' (Nothing, _) = exitBlock

        aquire' :: ReaderT Bar IO (c, Async (), Async ())
        aquire' = runSafeT $ do
          context <- aquire userSignalAction

          -- Start signalSource thread
          userTask <- barAsync $
            case signalThread of
              Just signalThread' -> signalThread' context userSignalAction
              Nothing -> return ()
          intervalTask <- barAsync intervalTimer

          return (context, userTask, intervalTask)


        release' :: (c, Async (), Async ()) -> ReaderT Bar IO ()
        release' (context, userTask, intervalTask) = do
          -- Signal block termination to render thread
          liftIO . atomically $ modifyTVar renderStateVar (_1 .~ Nothing)

          liftIO $ do
            cancel userTask
            cancel intervalTask

          runSafeT $ release context


        userSignalAction :: p -> IO ()
        userSignalAction value = do
          atomically $ writeTChan signalChan $ UserSignal value
          Event.set signalEvent

        mkBlockStateWithHandler :: Maybe BlockOutput -> BlockState
        mkBlockStateWithHandler Nothing = Nothing
        mkBlockStateWithHandler (Just output) = Just (output, Just signalEventHandler)

        signalPipe :: Client (Signal p) (Maybe BlockOutput) BarIO ExitBlock
        signalPipe = forever $ do
          -- Handle all queued events
          eventHandled <- sendQueuedSignals

          -- If there was no queued event signal a regular event
          unless eventHandled $ sendSignal RegularSignal

          -- Wait for next event
          liftIO $ Event.wait signalEvent
          liftIO $ Event.clear signalEvent

          where
            sendQueuedSignals :: Client (Signal p) (Maybe BlockOutput) BarIO Bool
            sendQueuedSignals = do
              maybeSignal <- liftIO . atomically $ tryReadTChan signalChan
              case maybeSignal of
                Just signal -> sendSignal signal >> sendQueuedSignals >> return True
                Nothing -> return False

            sendSignal :: Signal p -> Client (Signal p) (Maybe BlockOutput) BarIO ()
            sendSignal signal = do
              maybeOutput <- request signal

              let updateInvalidatedState = if isEventSignal signal then (_2 .~ False) else id

              let blockUpdate = (mkBlockStateWithHandler maybeOutput, signalToReason signal)
              liftIO . atomically $ modifyTVar renderStateVar ((_1 . _Just .~ blockUpdate) . updateInvalidatedState)
              liftIO $ Event.set renderEvent

            signalToReason :: Signal a -> BlockUpdateReason
            signalToReason (UserSignal _) = DefaultUpdate
            signalToReason (EventSignal _) = EventUpdate
            signalToReason RegularSignal = PollUpdate

            isEventSignal :: Signal p -> Bool
            isEventSignal (EventSignal _) = True
            isEventSignal _ = False


        intervalTimer :: BarIO ()
        intervalTimer = do
          scheduler <- askSleepScheduler
          case interval of
            Just interval' -> forever $ do
              sleepUntilInterval' scheduler interval'
              liftIO $ Event.set signalEvent
            Nothing -> return ()

        signalEventHandler :: BlockEventHandler
        signalEventHandler event = do
          wasInvalidatedBefore' <- liftIO . atomically $ do
            (_, wasInvalidatedBefore) <- readTVar renderStateVar
            unless wasInvalidatedBefore $ do
              writeTChan signalChan $ EventSignal event
              modifyTVar renderStateVar ((_2 .~ True) . (_1 . _Just . _2 .~ EventUpdate))
            return wasInvalidatedBefore

          unless wasInvalidatedBefore' $ liftIO $ do
            Event.set renderEvent
            Event.set signalEvent


-- |Converts a 'PollBlock' to a 'Block' by running it whenever the 'defaultInterval' is triggered.
runPollBlock :: PollBlock -> Block
runPollBlock = runPollBlock' defaultInterval

-- |Converts a 'PollBlock' to a 'Block' by running it whenever the provided 'Interval' is triggered.
runPollBlock' :: Interval -> PollBlock -> Block
runPollBlock' interval pb = do
  event <- liftIO Event.new
  pb >>~ addPollSignal >-> sleepToNextInterval event
  where
    addPollSignal :: a -> Proxy PollSignal a () a BarIO ExitBlock
    addPollSignal = respond >=> const (request PollSignal) >=> addPollSignal

    sleepToNextInterval :: Event.Event -> Pipe (Maybe BlockOutput) BlockUpdate BarIO ExitBlock
    sleepToNextInterval event = sleepToNextInterval' False
      where
        sleepToNextInterval' :: Bool -> Pipe (Maybe BlockOutput) BlockUpdate BarIO ExitBlock
        sleepToNextInterval' isEvent = do
          maybeOutput <- await
          -- Attach a click handler that will trigger a block update
          let state = mkBlockStateWithHandler (triggerOnClick event) maybeOutput
          yield (state, if isEvent then EventUpdate else PollUpdate)

          scheduler <- askSleepScheduler
          result <- liftIO $ do
            timerTask <- async $ sleepUntilInterval' scheduler interval
            eventTask <- async $ Event.wait event
            waitEitherCancel timerTask eventTask

          let isEventNew = isRight result

          when isEventNew $ do
            liftIO $ Event.clear event
            yield (invalidateBlockState state, EventUpdate)

          sleepToNextInterval' isEventNew


    mkBlockStateWithHandler :: BlockEventHandler -> Maybe BlockOutput -> BlockState
    mkBlockStateWithHandler _ Nothing = Nothing
    mkBlockStateWithHandler handler (Just output) = Just (output, Just handler)

    triggerOnClick :: Event.Event -> BlockEvent -> BarIO ()
    triggerOnClick event _ = liftIO $ Event.set event
