module QBar.Blocks.Date where

import QBar.BlockHelper
import QBar.BlockOutput
import QBar.Core
import QBar.Prelude
import QBar.Time

import qualified Data.Text.Lazy as T
import Data.Time.Format
import Data.Time.LocalTime


dateBlock :: Block
dateBlock = runPollBlock' (everyNSeconds 60) $ forever $ do
  zonedTime <- liftIO getZonedTime
  let logo = "📅\xFE0E "
  let date = T.pack (formatTime defaultTimeLocale "%a %F" zonedTime)
  let time = T.pack (formatTime defaultTimeLocale "%R" zonedTime)
  let text = normalText (logo <> date <> " ") <> activeText time
  let short = normalText logo <> activeText time
  yieldBlockUpdate $ (mkBlockOutput text) { _shortText = Just short }
