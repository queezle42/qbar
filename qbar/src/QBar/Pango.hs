module QBar.Pango (PangoText, renderPango) where

import QBar.Color
import QBar.Prelude
import QBar.Theme

type PangoText = Text

renderPango :: ThemedBlockText -> PangoText
renderPango (ThemedBlockText segments) = foldMap renderSegment segments
  where
    renderSegment :: ThemedBlockTextSegment -> PangoText
    renderSegment ThemedBlockTextSegment{themedSegmentText, color, backgroundColor} = coloredText backgroundColor color themedSegmentText


coloredText :: Maybe Color -> Color -> Text -> PangoText
coloredText Nothing foreground text = "<span color='" <> pangoColor foreground <> "'>" <> text <> "</span>"
coloredText (Just background) foreground text = "<span color='" <> pangoColor foreground <> "' background='" <> pangoColor background <> "'>" <> text <> "</span>"

pangoColor :: Color -> Text
pangoColor = hexColorText
