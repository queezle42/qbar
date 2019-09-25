{-# LANGUAGE OverloadedStrings #-}

module QBar.Blocks where

import QBar.Core
import QBar.Time

import qualified Data.Text.Lazy as T
import Data.Time.Format
import Data.Time.LocalTime
import Pipes
import Pipes.Concurrent

dateBlock :: IO Block
dateBlock = do
  zonedTime <- getZonedTime
  let date = T.pack (formatTime defaultTimeLocale "%a %F" zonedTime)
  let time = T.pack (formatTime defaultTimeLocale "%R" zonedTime)
  --let text = (T.pack "📅 ") <> T.pack (formatTime defaultTimeLocale "%a %F <span color='#ffffff'>%R</span>" zonedTime)
  let text = (T.pack "📅 ") <> date <> " " <> (coloredText activeColor time)
  return $ setBlockName "date" $ pangoMarkup $ createBlock text

dateBlockProducer :: BarUpdateChannel -> BlockProducer
dateBlockProducer barUpdateChannel = do
  initialDateBlock <- lift dateBlock
  (output, input) <- lift $ spawn $ latest initialDateBlock
  lift $ void $ forkIO $ update output
  fromInput input
  where
    update :: Output Block -> IO ()
    update output = do
      sleepUntil =<< nextMinute
      block <- dateBlock
      void $ atomically $ send output block
      updateBar barUpdateChannel
      update output

