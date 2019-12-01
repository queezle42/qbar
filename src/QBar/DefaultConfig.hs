module QBar.DefaultConfig where

import QBar.Blocks
import QBar.Core

import Control.Concurrent.Async
import Pipes

blockLocation :: String -> FilePath
blockLocation name = "~/.config/qbar/blocks/" <> name

generateDefaultBarConfig :: BarIO ()
generateDefaultBarConfig = do
  (systemInfoInterval, systemInfoIntervalTask) <- sharedInterval 10
  lift $ link systemInfoIntervalTask

  let todo = (systemInfoInterval $ blockScript $ blockLocation "todo")
  let wifi = (systemInfoInterval $ blockScript $ blockLocation "wifi2 wlan") >-> modify (addIcon "📡")
  let networkEnvironment = (systemInfoInterval $ blockScript $ blockLocation "network-environment")
  let cpu = (systemInfoInterval $ blockScript $ blockLocation "cpu_usage") >-> modify (setBlockName "cpu" . addIcon "💻") >-> autoPadding
  let ram = (systemInfoInterval $ blockScript $ blockLocation "memory") >-> modify (addIcon "🐏") >-> autoPadding
  let temperature = (systemInfoInterval $ blockScript $ blockLocation "temperature") >-> autoPadding
  volumeBlock <- startPersistentBlockScript $ blockLocation "volume-pulseaudio -S -F3"
  let battery = (systemInfoInterval $ blockScript $ blockLocation "battery2")

  addBlock dateBlock
  addBlock battery
  addBlock volumeBlock
  addBlock temperature
  addBlock ram
  addBlock cpu
  addBlock networkEnvironment
  addBlock wifi
  addBlock todo