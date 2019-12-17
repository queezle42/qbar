module QBar.Blocks.Date where

import QBar.Core
import QBar.Time
import QBar.BlockText

import qualified Data.Text.Lazy as T
import Data.Time.Format
import Data.Time.LocalTime
import Pipes
import Control.Lens


dateBlock :: PushBlock
dateBlock = do
  yield . Just =<< liftIO dateBlockOutput
  liftIO $ sleepUntil =<< nextMinute
  dateBlock


dateBlockOutput :: IO BlockOutput
dateBlockOutput = do
  zonedTime <- getZonedTime
  let date = T.pack (formatTime defaultTimeLocale "%a %F" zonedTime)
  let time = T.pack (formatTime defaultTimeLocale "%R" zonedTime)
  let text = normalText ("📅\xFE0E " <> date <> " ") <> activeText time
  return $ blockName ?~ "date" $ createBlock text
