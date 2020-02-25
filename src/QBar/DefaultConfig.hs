module QBar.DefaultConfig where

import QBar.Blocks
import QBar.BlockOutput
import QBar.Core

import Pipes

import Control.Lens

defaultBarConfig :: BarIO ()
defaultBarConfig = do
  systemInfoInterval <- sharedInterval 10

  let battery = systemInfoInterval $ batteryBlock >-> modify (blockName ?~ "battery")
  let cpuUsage = systemInfoInterval $ cpuUsageBlock 1 >-> modify ((blockName ?~ "cpuUsage") . addIcon "💻\xFE0E")

  -- TODO: commented-out blocks should be added as soon as they are implemented in qbar
  addBlock dateBlock
  addBlock battery
  --addBlock volumeBlock
  addBlock cpuUsage
  --addBlock ramUsageBlock
  --addBlock cpuTemperatureBlock
  --addBlock networkBlock

legacyBarConfig :: BarIO ()
legacyBarConfig = do
  systemInfoInterval <- sharedInterval 10

  let todo = systemInfoInterval (blockScript $ blockLocation "todo")
  let wifi = systemInfoInterval $ (blockScript $ blockLocation "wifi2") >-> modify (addIcon "📡\xFE0E")
  let networkEnvironment = systemInfoInterval (blockScript $ blockLocation "network-environment")
  let ram = systemInfoInterval $ (blockScript $ blockLocation "memory") >-> modify (addIcon "🐏\xFE0E") >-> autoPadding
  let temperature = systemInfoInterval $ (blockScript $ blockLocation "temperature") >-> autoPadding
  let volumeBlock = persistentBlockScript $ blockLocation "volume-pulseaudio -S -F3"
  let battery = systemInfoInterval $ batteryBlock >-> modify (blockName ?~ "battery")
  let cpuUsage = systemInfoInterval $ cpuUsageBlock 1 >-> modify ((blockName ?~ "cpuUsage") . addIcon "💻\xFE0E")

  addBlock dateBlock
  addBlock battery
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
