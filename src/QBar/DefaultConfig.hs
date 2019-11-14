module QBar.DefaultConfig where

import QBar.Blocks
import QBar.Core

import Control.Concurrent.Async
import Pipes

blockLocation :: String -> FilePath
blockLocation name = "~/.config/qbar/blocks/" <> name

generateDefaultBarConfig :: BarUpdateChannel -> IO [BlockProducer]
generateDefaultBarConfig barUpdateChannel = do
  (systemInfoInterval, systemInfoIntervalTask) <- sharedInterval barUpdateChannel 10
  link systemInfoIntervalTask
  --let irc = (systemInfoInterval $ blockScript "irc")
  let todo = (systemInfoInterval $ blockScript $ blockLocation "todo")
  let wifi = (systemInfoInterval $ blockScript $ blockLocation "wifi2 wlan") >-> modify (addIcon "📡")
  let networkEnvironment = (systemInfoInterval $ blockScript $ blockLocation "network-environment")
  let cpu = (systemInfoInterval $ blockScript $ blockLocation "cpu_usage") >-> modify (setBlockName "cpu" . addIcon "💻") >-> autoPadding
  let ram = (systemInfoInterval $ blockScript $ blockLocation "memory") >-> modify (addIcon "🐏") >-> autoPadding
  let temperature = (systemInfoInterval $ blockScript $ blockLocation "temperature") >-> autoPadding
  let volumeBlock = startPersistentBlockScript barUpdateChannel $ blockLocation "volume-pulseaudio -S -F3"
  let battery = (systemInfoInterval $ blockScript $ blockLocation "battery2")
  let date = dateBlockProducer barUpdateChannel
  return [todo, wifi, networkEnvironment, cpu, ram, temperature, volumeBlock, battery, date]