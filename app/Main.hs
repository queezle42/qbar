module Main where

import QBar.Cli
import QBar.ControlSocket
import QBar.DefaultConfig
import QBar.Filter
import QBar.Server

main :: IO ()
main = parseOptions >>= runQBar

runQBar :: MainOptions -> IO ()
runQBar options@MainOptions{barCommand} = runCommand barCommand
  where
    runCommand BarServer = runBarConfiguration generateDefaultBarConfig options
    runCommand NoFilter = sendIpc options $ SetFilter $ StaticFilter None
    runCommand RainbowFilter = sendIpc options $ SetFilter $ AnimatedFilter Rainbow