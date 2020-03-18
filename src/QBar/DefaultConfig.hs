module QBar.DefaultConfig where

import QBar.Blocks
import QBar.BlockOutput
import QBar.Core

import Pipes

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

legacyBarConfig :: BarIO ()
legacyBarConfig = do
  let ram = pollScriptBlock (blockLocation "memory") >-> modify (addIcon "🐏\xFE0E") >-> autoPadding
  let temperature = (pollScriptBlock $ blockLocation "temperature") >-> autoPadding
  let volumeBlock = scriptBlock $ blockLocation "volume-pulseaudio -S -F3"

  addBlock dateBlock
  addBlock batteryBlock
  addBlock volumeBlock
  addBlock $ cpuUsageBlock 1
  addBlock ram
  addBlock temperature
  addBlock networkManagerBlock
  where
    blockLocation :: String -> FilePath
    blockLocation name = "~/.config/qbar/blocks/" <> name
