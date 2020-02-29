module QBar.DefaultConfig where

import QBar.Blocks
import QBar.BlockOutput
import QBar.Core

import Pipes

defaultBarConfig :: BarIO ()
defaultBarConfig = do
  let cpuUsage = cpuUsageBlock 1 >-> modify (addIcon "💻\xFE0E")

  -- TODO: commented-out blocks should be added as soon as they are implemented in qbar
  addBlock dateBlock
  addBlock batteryBlock
  --addBlock volumeBlock
  addBlock cpuUsage
  --addBlock ramUsageBlock
  --addBlock cpuTemperatureBlock
  --addBlock networkBlock

legacyBarConfig :: BarIO ()
legacyBarConfig = do
  let todo = scriptBlock $ blockLocation "todo"
  let wifi = (scriptBlock $ blockLocation "wifi2") >-> modify (addIcon "📡\xFE0E")
  let networkEnvironment = scriptBlock $ blockLocation "network-environment"
  let ram = (scriptBlock $ blockLocation "memory") >-> modify (addIcon "🐏\xFE0E") >-> autoPadding
  let temperature = (scriptBlock $ blockLocation "temperature") >-> autoPadding
  let volumeBlock = persistentScriptBlock $ blockLocation "volume-pulseaudio -S -F3"
  let cpuUsage = cpuUsageBlock 1 >-> modify (addIcon "💻\xFE0E")

  addBlock dateBlock
  addBlock batteryBlock
  addBlock volumeBlock
  addBlock temperature
  addBlock ram
  addBlock cpuUsage
  addBlock networkEnvironment
  addBlock wifi
  addBlock todo
  where
    blockLocation :: String -> FilePath
    blockLocation name = "~/.config/qbar/blocks/" <> name
