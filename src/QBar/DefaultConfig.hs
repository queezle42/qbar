module QBar.DefaultConfig where

import QBar.Blocks
import QBar.BlockOutput
import QBar.Core

import Pipes

import Control.Lens

blockLocation :: String -> FilePath
blockLocation name = "~/.config/qbar/blocks/" <> name

generateDefaultBarConfig :: BarIO ()
generateDefaultBarConfig = do
  systemInfoInterval <- sharedInterval 10

  let todo = systemInfoInterval (blockScript $ blockLocation "todo")
  let wifi = systemInfoInterval (blockScript $ blockLocation "wifi2") >-> modify (addIcon "📡\xFE0E")
  let networkEnvironment = systemInfoInterval (blockScript $ blockLocation "network-environment")
  let cpu = systemInfoInterval (blockScript $ blockLocation "cpu_usage") >-> modify ((blockName?~"cpu") . addIcon "💻\xFE0E") >-> autoPadding
  let ram = systemInfoInterval (blockScript $ blockLocation "memory") >-> modify (addIcon "🐏\xFE0E") >-> autoPadding
  let temperature = systemInfoInterval (blockScript $ blockLocation "temperature") >-> autoPadding
  let volumeBlock = startPersistentBlockScript $ blockLocation "volume-pulseaudio -S -F3"
  let battery = systemInfoInterval $ batteryBlock >-> modify (blockName?~"battery")

  addBlock dateBlock
  addBlock battery
  addBlock volumeBlock
  addBlock temperature
  addBlock ram
  addBlock cpu
  addBlock networkEnvironment
  addBlock wifi
  addBlock todo
