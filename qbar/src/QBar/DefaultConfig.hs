module QBar.DefaultConfig where

import QBar.Blocks
import QBar.Core
import QBar.Prelude

defaultBarConfig :: BarIO ()
defaultBarConfig = do
  -- TODO: commented-out blocks should be added as soon as they are implemented in qbar
  addBlock dateBlock
  addBlock batteryBlock
  --addBlock volumeBlock
  addBlock $ cpuUsageBlock 1
  --addBlock ramUsageBlock
  --addBlock freeDiskSpaceBlock
  --addBlock cpuTemperatureBlock
  addBlock networkManagerBlock
