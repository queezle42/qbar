{-# LANGUAGE RankNTypes #-}

module QBar.BlockHelper where

import QBar.BlockOutput
import QBar.Core
import QBar.Time

import Control.Concurrent.Async
import qualified Control.Concurrent.Event as Event
import Control.Concurrent.STM.TChan
import Control.Monad.State (StateT, evalStateT, get, put)
import Data.Either (isRight)
import Pipes
import Pipes.Concurrent
import Pipes.Core

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

  runSignalBlockWithThreadInternal signalChan signalEvent
  where
    runSignalBlockWithThreadInternal :: TChan (Signal p) -> Event.Event -> Block
    runSignalBlockWithThreadInternal signalChan signalEvent = do
      context <- lift $ aquire userSignalAction
      -- Start signalSource thread
      userTask <- liftBarIO $ barAsync $
        case signalThread of
          Just signalThread' -> signalThread' context userSignalAction
          Nothing -> return ()
      intervalTask <- liftBarIO $ barAsync intervalTimer

      -- Run block
      void (signalBlock context +>> signalPipe)

      -- Cancel threads when the block terminates
      -- TODO: use bracketP?
      liftIO $ do
        cancel userTask
        cancel intervalTask

      liftBarIO $ release context

      exitBlock

      where
        userSignalAction :: p -> IO ()
        userSignalAction value = do
          liftIO . atomically $ writeTChan signalChan $ UserSignal value
          Event.set signalEvent

        signalPipe :: Proxy (Signal p) (Maybe BlockOutput) () BlockUpdate BarIO ExitBlock
        signalPipe = do
          initial <- request RegularSignal
          let initialUpdate = (mkBlockStateWithHandler initial, PollUpdate)
          yield initialUpdate
          evalStateT stateSignalPipe initialUpdate

        mkBlockStateWithHandler :: Maybe BlockOutput -> BlockState
        mkBlockStateWithHandler Nothing = Nothing
        mkBlockStateWithHandler (Just output) = Just (output, Just signalEventHandler)

        stateSignalPipe :: StateT BlockUpdate (Proxy (Signal p) (Maybe BlockOutput) () BlockUpdate BarIO) ExitBlock
        stateSignalPipe = forever $ do
          -- Handle all queued events
          eventHandled <- sendQueuedEvents

          -- If there was no queued event signal a regular event
          unless eventHandled $ outputAndStore RegularSignal

          -- Wait for next event
          liftIO $ Event.wait signalEvent
          liftIO $ Event.clear signalEvent

          where
            sendQueuedEvents :: StateT BlockUpdate (Proxy (Signal p) (Maybe BlockOutput) () BlockUpdate BarIO) Bool
            sendQueuedEvents = do
              maybeSignal <- liftIO . atomically $ tryReadTChan signalChan
              case maybeSignal of
                Just signal -> do
                  case signal of
                    EventSignal _ -> do
                      (state, _) <- get
                      lift $ yield (invalidateBlockState state, EventUpdate)
                    _ -> return ()
                  outputAndStore signal
                  void sendQueuedEvents
                  return True
                Nothing -> return False

            outputAndStore :: Signal p -> StateT BlockUpdate (Proxy (Signal p) (Maybe BlockOutput) () BlockUpdate BarIO) ()
            outputAndStore signal = do
              maybeOutput <- lift $ request signal
              let update = (mkBlockStateWithHandler maybeOutput, signalToReason signal)
              put update
              lift $ yield update

            signalToReason :: Signal a -> BlockUpdateReason
            signalToReason (UserSignal _) = DefaultUpdate
            signalToReason (EventSignal _) = EventUpdate
            signalToReason RegularSignal = PollUpdate


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
          liftIO . atomically $ writeTChan signalChan $ EventSignal event
          liftIO $ Event.set signalEvent


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
