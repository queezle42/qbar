module QBar.Blocks.Date where

import QBar.BlockOutput
import QBar.Core
import QBar.Time

import qualified Data.Text.Lazy as T
import Data.Time.Format
import Data.Time.LocalTime


dateBlock :: PushBlock
dateBlock = schedulePullBlock' (everyNSeconds 60) $ forever $ do
  zonedTime <- liftIO getZonedTime
  let date = T.pack (formatTime defaultTimeLocale "%a %F" zonedTime)
  let time = T.pack (formatTime defaultTimeLocale "%R" zonedTime)
  let text = normalText ("📅\xFE0E " <> date <> " ") <> activeText time
  updateBlock $ mkBlockOutput text
