module QBar.Util where

import Control.Concurrent.Event as Event
import Pipes

-- Pipe that signals an 'Event' after every value that passes through
signalPipe :: MonadIO m => Event.Event -> Pipe a a m r
signalPipe event = signalPipe'
  where
    signalPipe' :: MonadIO m => Pipe a a m r
    signalPipe' = do
      value <- await
      yield value
      liftIO $ Event.signal event
      signalPipe'
