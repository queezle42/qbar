module QBar.Pango (PangoText, renderPango) where

import QBar.Theme

import Data.Colour.RGBSpace
import qualified Data.Text.Lazy as T
import Numeric (showHex)

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
pangoColor = pangoColor'
  where
    pangoColor' :: Color -> Text
    pangoColor' (ColorRGB rgb) = pangoRGB rgb
    pangoColor' (ColorRGBA rgb a) = pangoRGB rgb <> hexColorComponent a

    pangoRGB :: RGB Double -> Text
    pangoRGB (RGB r g b) =
      let r' = hexColorComponent r
          g' = hexColorComponent g
          b' = hexColorComponent b
      in "#" <> r' <> g' <> b'
    hexColorComponent :: Double -> Text
    hexColorComponent val = paddedHexComponent $ T.pack $ showHex (max 0 $ min 255 (truncate (val * 255) :: Int)) ""
    paddedHexComponent :: Text -> Text
    paddedHexComponent hex =
      let len = 2 - T.length hex
          padding = if len == 1 then "0" else ""
      in padding <> hex
