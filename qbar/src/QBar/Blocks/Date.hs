module QBar.Blocks.Date (
  dateBlock,
) where

import QBar.BlockHelper
import QBar.BlockOutput
import QBar.Core
import QBar.Prelude
import QBar.Time

import Data.Time.Format
import Data.Time.LocalTime
import Data.Text.Lazy qualified as T


dateBlock :: Block
dateBlock = runPollBlock' (everyNSeconds 60) $ forever $ do
  zonedTime <- liftIO getZonedTime
  let logo :: Text = "📅\xFE0E "
  let date = T.pack (formatTime defaultTimeLocale "%a %F" zonedTime)
  let time = T.pack (formatTime defaultTimeLocale "%R" zonedTime)
  let text = normalText (logo <> date <> " ") <> activeText time
  let short = normalText logo <> activeText time
  yieldBlockUpdate $ (mkBlockOutput text) { _shortText = Just short }
