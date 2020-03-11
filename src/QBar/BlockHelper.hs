{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

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

type SignalBlock a = (Signal a -> Server (Signal a) BlockUpdate BarIO ExitBlock)

-- |Block that 'respond's with an update whenever it receives a 'PollSignal'.
type PollBlock' = Server PollSignal BlockUpdate BarIO
type PollBlock = Server PollSignal BlockUpdate BarIO ExitBlock
data PollSignal = PollSignal

respondBlockUpdate :: BlockOutput -> Server' (Signal s) BlockUpdate BarIO (Signal s)
respondBlockUpdate blockOutput = respond (Just (blockOutput, Nothing), DefaultUpdate)

respondBlockUpdate' :: BlockEventHandler -> BlockOutput -> Server' (Signal s) BlockUpdate BarIO (Signal s)
respondBlockUpdate' blockEventHandler blockOutput = respond (Just (blockOutput, Just blockEventHandler), PollUpdate)

-- |Update a block by removing the current output
respondEmptyBlockUpdate :: Server' (Signal s) BlockUpdate BarIO (Signal s)
respondEmptyBlockUpdate = respond (Nothing, PollUpdate)


yieldBlockUpdate :: BlockOutput -> Server' PollSignal BlockUpdate BarIO ()
yieldBlockUpdate blockOutput = void . respond $ (Just (blockOutput, Nothing), PollUpdate)

yieldBlockUpdate' :: BlockEventHandler -> BlockOutput -> Server' PollSignal BlockUpdate BarIO ()
yieldBlockUpdate' blockEventHandler blockOutput = void . respond $ (Just (blockOutput, Just blockEventHandler), PollUpdate)

-- |Update a block by removing the current output
yieldEmptyBlockUpdate :: Server' PollSignal BlockUpdate BarIO ()
yieldEmptyBlockUpdate = void . respond $ (Nothing, PollUpdate)


runSignalBlock :: forall a. Maybe Interval -> Maybe ((a -> IO ()) -> BarIO ()) -> SignalBlock a -> Block
runSignalBlock maybeInterval maybeSignalSourceThread signalBlock' = runSignalBlockConfiguration $ SignalBlockConfiguration {
  initialize = const $ return (),
  signalThread = const <$> maybeSignalSourceThread,
  signalBlock = const signalBlock',
  interval = maybeInterval,
  finalize = return
}


runSignalBlockFn :: forall a. Maybe Interval -> ((a -> IO ()) -> BarIO ()) -> ((a, Maybe BlockEvent) -> BarIO BlockState) -> Block
runSignalBlockFn maybeInterval signalSourceThread renderFn = runSignalBlock maybeInterval (Just signalSourceThread) signalBlock
  where
    signalBlock :: (Signal a -> Server (Signal a) BlockUpdate BarIO ExitBlock)
    signalBlock (UserSignal value) = signalBlock' value (UserSignal value)
    signalBlock _ = signalBlock =<< respondEmptyBlockUpdate
    signalBlock' :: a -> (Signal a -> Server (Signal a) BlockUpdate BarIO ExitBlock)
    signalBlock' state RegularSignal = signalBlock' state =<< respond =<< (, PollUpdate) <$> lift (renderFn (state, Nothing))
    signalBlock' _ (UserSignal value) = signalBlock' value =<< respond =<< (, DefaultUpdate) <$> lift (renderFn (value, Nothing))
    signalBlock' state (EventSignal event) = signalBlock' state =<< respond =<< (, DefaultUpdate) <$> lift (renderFn (state, Just event))

runSignalBlockFn' :: Maybe Interval -> (Maybe BlockEvent -> BarIO BlockState) -> Block
runSignalBlockFn' maybeInterval renderFn = runSignalBlockConfiguration $ SignalBlockConfiguration {
  initialize = const $ return (),
  signalThread = Nothing,
  signalBlock = const eventBlock,
  interval = maybeInterval,
  finalize = return
}
  where
    eventBlock :: (Signal a -> Server (Signal a) BlockUpdate BarIO ExitBlock)
    eventBlock (EventSignal event) = eventBlock =<< respond =<< (, DefaultUpdate) <$> lift (renderFn (Just event))
    eventBlock _ = eventBlock =<< respond =<< (, PollUpdate) <$> lift (renderFn Nothing)



data SignalBlockConfiguration c p = SignalBlockConfiguration {
  initialize :: (p -> IO ()) -> BarIO c,
  signalThread :: Maybe (c -> (p -> IO ()) -> BarIO ()),
  signalBlock :: c -> SignalBlock p,
  interval :: Maybe Interval,
  finalize :: c -> BarIO ()
}

runSignalBlockConfiguration :: forall c p. SignalBlockConfiguration c p -> Block
runSignalBlockConfiguration SignalBlockConfiguration{initialize, signalThread, signalBlock, interval, finalize} = do
  -- Initialize
  signalChan <- liftIO newTChanIO
  signalEvent <- liftIO Event.new

  runSignalBlockWithThreadInternal signalChan signalEvent
  where
    runSignalBlockWithThreadInternal :: TChan (Signal p) -> Event.Event -> Block
    runSignalBlockWithThreadInternal signalChan signalEvent = do
      context <- lift $ initialize userSignalAction
      -- Start signalSource thread
      userTask <- liftBarIO $ barAsync $
        case signalThread of
          Just signalThread' -> signalThread' context userSignalAction
          Nothing -> return ()
      intervalTask <- liftBarIO $ barAsync intervalTimer

      -- Run block
      void (signalBlock context +>> signalPipe >-> attachEventHandlerP)

      -- Cancel threads when the block terminates
      -- TODO: use bracketP?
      liftIO $ do
        cancel userTask
        cancel intervalTask

      liftBarIO $ finalize context

      exitBlock

      where
        userSignalAction :: p -> IO ()
        userSignalAction value = do
          liftIO . atomically $ writeTChan signalChan $ UserSignal value
          Event.set signalEvent

        signalPipe :: Proxy (Signal p) BlockUpdate () BlockUpdate BarIO ExitBlock
        signalPipe = do
          initial <- request RegularSignal
          yield initial
          evalStateT stateSignalPipe initial
        stateSignalPipe :: StateT BlockUpdate (Proxy (Signal p) BlockUpdate () BlockUpdate BarIO) ExitBlock
        stateSignalPipe = forever $ do
          -- Handle all queued events
          eventHandled <- sendQueuedEvents

          -- If there was no queued event signal a regular event
          unless eventHandled $ outputAndStore RegularSignal

          -- Wait for next event
          liftIO $ Event.wait signalEvent
          liftIO $ Event.clear signalEvent

          where
            sendQueuedEvents :: StateT BlockUpdate (Proxy (Signal p) BlockUpdate () BlockUpdate BarIO) Bool
            sendQueuedEvents = do
              maybeSignal <- liftIO . atomically $ tryReadTChan signalChan
              case maybeSignal of
                Just signal -> do
                  case signal of
                    EventSignal _ -> do
                      (state, _) <- get
                      lift $ yield (invalidateBlockState state, UserUpdate)
                    _ -> return ()
                  outputAndStore signal
                  void $ sendQueuedEvents
                  return True
                Nothing -> return False

            outputAndStore :: Signal p -> StateT BlockUpdate (Proxy (Signal p) BlockUpdate () BlockUpdate BarIO) ()
            outputAndStore signal = do
              value <- lift $ request signal
              put value
              lift $ yield value


        intervalTimer :: BarIO ()
        intervalTimer = do
          scheduler <- askSleepScheduler
          case interval of
            Just interval' -> forever $ do
              sleepUntilInterval' scheduler interval'
              liftIO $ Event.set signalEvent
            Nothing -> return ()

        attachEventHandlerP :: Pipe BlockUpdate BlockUpdate BarIO ExitBlock
        attachEventHandlerP = forever $ do
          (state, reason) <- await
          let state' = if hasEventHandler state
              -- If state already has an event handler, we do not attach another one
              then state
              -- Attach a click handler that will trigger a block update
              else updateEventHandler signalEventHandler state
          yield (state', reason)
          where
            signalEventHandler :: BlockEventHandler
            signalEventHandler event = do
              liftIO . atomically $ writeTChan signalChan $ EventSignal event
              liftIO $ Event.set signalEvent


-- |Converts a 'PollBlock' to a 'Block' by running it whenever the 'defaultInterval' is triggered.
runPollBlock :: PollBlock -> Block
runPollBlock = runPollBlock' defaultInterval

-- |Converts a 'PollBlock' to a 'Block' by running it whenever the provided 'Interval' is triggered.
runPollBlock' :: Interval -> PollBlock -> Block
runPollBlock' interval pb = pb >>~ addPollSignal >-> sleepToNextInterval
  where
    addPollSignal :: BlockUpdate -> Proxy PollSignal BlockUpdate () BlockUpdate BarIO ExitBlock
    addPollSignal = respond >=> const (request PollSignal) >=> addPollSignal

    sleepToNextInterval :: Pipe BlockUpdate BlockUpdate BarIO ExitBlock
    sleepToNextInterval = do
      event <- liftIO Event.new
      forever $ do
        (state, reason) <- await
        if hasEventHandler state
          then do
            -- If state already has an event handler, we do not attach another one
            yield (state, reason)
            sleepUntilInterval interval
          else do
            -- Attach a click handler that will trigger a block update
            yield (updateEventHandler (triggerOnClick event) state, reason)

            scheduler <- askSleepScheduler
            result <- liftIO $ do
              timerTask <- async $ sleepUntilInterval' scheduler defaultInterval
              eventTask <- async $ Event.wait event
              waitEitherCancel timerTask eventTask

            when (isRight result) $ do
              liftIO $ Event.clear event
              yield (invalidateBlockState state, UserUpdate)

    triggerOnClick :: Event.Event -> BlockEvent -> BarIO ()
    triggerOnClick event _ = liftIO $ Event.set event
