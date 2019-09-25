module QBar.DefaultConfig where

import QBar.Blocks
import QBar.Core

import Control.Concurrent.Async
import Pipes

generateDefaultBarConfig :: BarUpdateChannel -> IO [BlockProducer]
generateDefaultBarConfig barUpdateChannel = do
  (systemInfoInterval, systemInfoIntervalTask) <- sharedInterval barUpdateChannel 10
  link systemInfoIntervalTask
  --let irc = (systemInfoInterval $ blockScript "/home/jens/run/blocks/irc")
  let todo = (systemInfoInterval $ blockScript "/home/jens/run/blocks/todo")
  let wifi = (systemInfoInterval $ blockScript "/home/jens/run/blocks/wifi2 wlan") >-> modify (addIcon "📡")
  let networkEnvironment = (systemInfoInterval $ blockScript "/home/jens/run/blocks/network-environment")
  let cpu = (systemInfoInterval $ blockScript "/home/jens/run/blocks/cpu_usage") >-> modify (setBlockName "cpu" . addIcon "💻") >-> autoPadding
  let ram = (systemInfoInterval $ blockScript "/home/jens/run/blocks/memory") >-> modify (addIcon "🐏") >-> autoPadding
  let temperature = (systemInfoInterval $ blockScript "/home/jens/run/blocks/temperature") >-> autoPadding
  let volumeBlock = startPersistentBlockScript barUpdateChannel "/home/jens/run/blocks/volume-pulseaudio -S -F3"
  let battery = (systemInfoInterval $ blockScript "/home/jens/run/blocks/battery2")
  let date = dateBlockProducer barUpdateChannel
  return [todo, wifi, networkEnvironment, cpu, ram, temperature, volumeBlock, battery, date]