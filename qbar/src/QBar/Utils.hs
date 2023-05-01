module QBar.Utils where

import QBar.Prelude

import Control.Concurrent.Event as Event
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Monad (replicateM)
import qualified Data.Text.Lazy as T
import Pipes
import System.Random

-- Pipe that signals an 'Event' after every value that passes through
signalEventPipe :: MonadIO m => Event.Event -> Pipe a a m r
signalEventPipe event = forever $ (yield =<< await) >> liftIO (Event.signal event)

randomIdentifier :: MonadIO m => m Text
randomIdentifier = liftIO $ T.pack <$> replicateM 8 randomCharacter
  where
    randomCharacter :: IO Char
    randomCharacter = do
      index <- randomRIO (0, T.length alphabet - 1)
      return $ T.index alphabet index
    alphabet :: T.Text
    alphabet = T.pack $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

-- |Creates a pair of consumer and producer. Both can be used multiple times in parallel.
-- |All values send to a consumer will be sent to all currently running producers.
mkBroadcastP :: forall a. IO (Consumer a IO (), Producer a IO ())
mkBroadcastP = do
  chan <- newBroadcastTChanIO
  return (sendToStore chan, recvFromStore chan)
  where
    sendToStore :: TChan a -> Consumer a IO ()
    sendToStore chan = forever $ do
      value <- await
      liftIO . atomically $ writeTChan chan value

    -- Monad will be forked when new outputs connect
    recvFromStore :: TChan a -> Producer a IO ()
    recvFromStore chan = do
      outputChan <- liftIO . atomically $ dupTChan chan
      forever $ yield =<< (liftIO . atomically $ readTChan outputChan)

-- |Creates a pair of consumer and producer. Both can be used multiple times in parallel.
-- |All values send to a consumer will be sent to all currently running producers.
-- |When running a new producer it will immediateley receive the latest value that was sent to a consumer.
mkBroadcastCacheP :: forall a. a -> IO (Consumer a IO (), Producer a IO ())
mkBroadcastCacheP initialValue = do
  store <- (,) <$> newTVarIO initialValue <*> newBroadcastTChanIO
  return (sendToStore store, recvFromStore store)
  where
    sendToStore :: (TVar a, TChan a) -> Consumer a IO ()
    sendToStore (var, chan) = forever $ do
      value <- await
      liftIO . atomically $ do
        writeTVar var value
        writeTChan chan value

    -- Monad will be forked when new outputs connect
    recvFromStore :: (TVar a, TChan a) -> Producer a IO ()
    recvFromStore (var, chan) = do
      (outputChan, value) <- liftIO . atomically $ do
        value <- readTVar var
        outputChan <- dupTChan chan
        return (outputChan, value)

      yield value

      forever $ yield =<< (liftIO . atomically $ readTChan outputChan)
