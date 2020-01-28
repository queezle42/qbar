module QBar.Host where

import QBar.Core

import Control.Concurrent.Event as Event
import Control.Concurrent.STM.TChan (TChan, newTChanIO)

runBarHost :: (TChan CachedBlock -> BarUpdateEvent -> BarIO ()) -> IO ()
runBarHost host = do
  -- Create an event used to signal bar updates
  barUpdateEvent <- Event.newSet
  let requestBarUpdate = Event.set barUpdateEvent

  -- Create channel to send new block producers to render loop
  newBlockChan <- newTChanIO

  let bar = Bar { requestBarUpdate, newBlockChan }
  runBarIO bar (host newBlockChan barUpdateEvent)
