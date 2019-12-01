{-# LANGUAGE OverloadedStrings #-}

module QBar.Blocks where

import QBar.Core
import QBar.Time

import qualified Data.Text.Lazy as T
import Data.Time.Format
import Data.Time.LocalTime
import Pipes

dateBlock :: PushBlock
dateBlock = do
  yield =<< liftIO dateBlockOutput
  liftIO $ sleepUntil =<< nextMinute
  dateBlock

dateBlockOutput :: IO BlockOutput
dateBlockOutput = do
  zonedTime <- getZonedTime
  let date = T.pack (formatTime defaultTimeLocale "%a %F" zonedTime)
  let time = T.pack (formatTime defaultTimeLocale "%R" zonedTime)
  let text = (T.pack "📅 ") <> date <> " " <> (coloredText activeColor time)
  return $ setBlockName "date" $ pangoMarkup $ createBlock text
