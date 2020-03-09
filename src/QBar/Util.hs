module QBar.Util where

import Control.Concurrent.Event as Event
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
