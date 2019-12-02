module Main where

import QBar.Cli
import QBar.DefaultConfig
import QBar.Server

main :: IO ()
main = parseOptions >>= runQBar generateDefaultBarConfig