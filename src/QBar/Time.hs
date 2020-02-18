module QBar.Time (sleepUntil, nextMinute) where

import Control.Concurrent (threadDelay)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime, utctDayTime)

sleepUntil :: UTCTime -> IO ()
sleepUntil time = do
  now <- getCurrentTime
  let timeUntil = diffUTCTime time now
  when (timeUntil > 0) $
    if timeUntil > 1
      then threadDelay 1000000 >> sleepUntil time
      else threadDelay $ ceiling $ toRational timeUntil * 1000000

nextMinute :: IO UTCTime
nextMinute = do
  now <- getCurrentTime
  let dayTime = utctDayTime now
  let daySeconds = floor dayTime
  let dayMinute = daySeconds `div` 60
  return now {
    utctDayTime = fromInteger $ (dayMinute + 1) * 60
  }