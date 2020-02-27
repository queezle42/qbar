{-# LANGUAGE OverloadedLists #-}

module QBar.Time (SleepScheduler, HasSleepScheduler(..), Interval, createSleepScheduler, sleepUntil, sleepUntil', sleepUntilInterval, sleepUntilInterval', everyMinute, everyNSeconds, nextIntervalTime) where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import qualified Control.Concurrent.Event as Event
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime, utctDayTime, addUTCTime)
import Data.SortedList (SortedList, toSortedList, fromSortedList, singleton, partition, insert)
import Data.Ord (comparing)

newtype Interval = IntervalSeconds Integer

-- |Describes an interval that is run every "n" seconds after midnight.
everyNSeconds :: Integer -> Interval
everyNSeconds = IntervalSeconds

-- |Describes an interval that is run every minute.
everyMinute :: Interval
everyMinute = IntervalSeconds 60

nextIntervalTime :: MonadIO m => Interval -> m UTCTime
nextIntervalTime (IntervalSeconds intervalSeconds) = liftIO $ do
  now <- getCurrentTime
  let dayTime = utctDayTime now
  let daySeconds = floor dayTime
  let intervalId = daySeconds `div` intervalSeconds
  return now {
    utctDayTime = fromInteger $ (intervalId + 1) * intervalSeconds
  }


data SleepScheduler = SleepScheduler (MVar (SortedList ScheduledEvent, [ScheduledEvent])) Event.Event
data ScheduledEvent = ScheduledEvent {
  time :: UTCTime,
  event :: Event.Event,
  fireOnNegativeTimeJump :: Bool
} deriving Eq
instance Ord ScheduledEvent where
  compare = comparing time

class HasSleepScheduler m where
  askSleepScheduler :: m SleepScheduler

createSleepScheduler :: MonadIO m => m SleepScheduler
createSleepScheduler = liftIO $ do
  scheduler <- SleepScheduler <$> newMVar ([], []) <*> Event.new
  link =<< (async $ schedulerThread scheduler)
  return scheduler
  where
    schedulerThread :: SleepScheduler -> IO ()
    schedulerThread (SleepScheduler eventsMVar trigger) = schedulerThread' =<< getCurrentTime
      where
        schedulerThread' :: UTCTime -> IO ()
        schedulerThread' lastTime = do
          start <- getCurrentTime

          -- Check for a negative time step (threshold is between 5 and 65 seconds, depending on loop activity)
          when (start < addUTCTime (fromInteger (-5)) lastTime) $ fireEvents fireOnNegativeTimeJump

          (sortedEvents, _) <- readMVar eventsMVar
          waitResult <- case fromSortedList sortedEvents of
            [] -> True <$ Event.wait trigger
            (ScheduledEvent{time} : _) -> waitForEvent time

          when waitResult $ do
            now <- getCurrentTime
            fireEvents (checkEvent now)

          schedulerThread' start

        -- |Waits for the next event, with a timeout. A return value of 'False' indicates a timeout occured.
        waitForEvent :: UTCTime -> IO Bool
        waitForEvent eventTime = do
          now <- getCurrentTime
          let timeUntil = diffUTCTime eventTime now
          if
            | timeUntil <= 0 -> return True
            | timeUntil < 60 -> True <$ Event.waitTimeout trigger (ceiling $ toRational timeUntil * 1000000)
            -- False indicates a timeout, in which case no events need to be fired
            | otherwise -> Event.waitTimeout trigger (60 * 1000000)


        fireEvents :: (ScheduledEvent -> Bool) -> IO ()
        fireEvents predicate =
          modifyMVar_ eventsMVar $ \(hots, colds) -> do
            let allEvents = hots <> toSortedList colds
            let (activeEvents, futureEvents) = partition predicate allEvents
            mapM_ (Event.set . event) activeEvents
            -- Sleep scheduler thread 'Event' is cleared during 'modifyMVar_' to prevent race conditions.
            Event.clear trigger
            return (futureEvents, [])

        -- |Predicate to check if an event should be fired.
        checkEvent :: UTCTime -> ScheduledEvent -> Bool
        checkEvent now ScheduledEvent{time} = now >= time


queueScheduledEvent :: MonadIO m => SleepScheduler -> ScheduledEvent -> m ()
queueScheduledEvent (SleepScheduler eventsMVar trigger) event@ScheduledEvent{time=eventTime} = liftIO $
  modifyMVar_ eventsMVar $ \(sorted, unsorted) ->
    -- Sleep scheduler thread 'Event' is set during 'modifyMVar_' to prevent race conditions.
    case fromSortedList sorted of
      [] -> (singleton event, unsorted) <$ Event.set trigger
      (first : _) ->
        if eventTime < time first
          -- Event happens before the first event, so it is inserted at the front of the sorted list and the scheduler thread is notified
          then (insert event sorted, unsorted) <$ Event.set trigger
          -- Otherwise it is added to the unsorted pool and will be handled later.
          else return (sorted, event:unsorted)


-- |Suspends the thread until the given time is reached.
sleepUntil :: (HasSleepScheduler m, MonadIO m) => UTCTime -> m ()
sleepUntil time = do
  scheduler <- askSleepScheduler
  sleepUntil' scheduler time

sleepUntil' :: MonadIO m => SleepScheduler -> UTCTime -> m ()
sleepUntil' scheduler time = liftIO $ do
  event <- Event.new
  queueScheduledEvent scheduler (ScheduledEvent {time, event, fireOnNegativeTimeJump=False})
  Event.wait event

-- |Suspends the thread until the next time boundary described by 'Interval' is reached. Also returns when the system time jumps backwards.
sleepUntilInterval :: (HasSleepScheduler m, MonadIO m) => Interval -> m ()
sleepUntilInterval interval = do
  scheduler <- askSleepScheduler
  sleepUntilInterval' scheduler interval

sleepUntilInterval' :: MonadIO m => SleepScheduler -> Interval -> m ()
sleepUntilInterval' scheduler interval = liftIO $ do
  event <- Event.new
  time <- nextIntervalTime interval
  queueScheduledEvent scheduler (ScheduledEvent {time, event, fireOnNegativeTimeJump=True})
  Event.wait event
