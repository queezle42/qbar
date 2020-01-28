module QBar.Themes where

import QBar.BlockOutput
import QBar.BlockText

import qualified Data.Text.Lazy as T

import Control.Lens


type Theme = [BlockOutput] -> [(T.Text, Maybe T.Text)]
type SimplifiedTheme = Bool -> Importance -> (Color, Maybe Color)
type AnimatedTheme = Double -> Theme


mkTheme :: SimplifiedTheme -> Theme
mkTheme theming' = map themeBlock
  where
    themeBlock :: BlockOutput -> (T.Text, Maybe T.Text)
    themeBlock block = (fullText', shortText')
      where
        theming :: SimplifiedTheme
        theming
          | block^.invalid = invalidSimplifiedTheme
          | otherwise = theming'
        fullText' :: T.Text
        fullText' = themeBlockText theming $ block^.fullText
        shortText' :: Maybe T.Text
        shortText' = themeBlockText theming <$> block^.shortText
    themeBlockText :: SimplifiedTheme -> BlockText -> T.Text
    themeBlockText theming (BlockText b) = foldr ((<>) . themeSegment theming) "" b
    themeSegment :: SimplifiedTheme -> BlockTextSegment -> T.Text
    themeSegment theming BlockTextSegment {active, importance, text} = (applyTheme $ theming active importance) text
    themeSegment _ (PangoTextSegment text) = text
    applyTheme :: (Color, Maybe Color) -> T.Text -> T.Text
    applyTheme (fc, Just bc) s = "<span color='" <> colorToHex fc <> "' background='" <> colorToHex bc <> "'>" <> s <> "</span>"
    applyTheme (fc, Nothing) s = "<span color='" <> colorToHex fc <> "'>" <> s <> "</span>"


invalidColor :: Color
invalidColor = ColorRGBA (0x96/255) (0x98/255) (0x96/255) (0x77/255)


invalidSimplifiedTheme :: SimplifiedTheme
invalidSimplifiedTheme _ _ = (invalidColor, Nothing)


invalidTheme :: Theme
invalidTheme = mkTheme invalidSimplifiedTheme


defaultTheme :: Theme
defaultTheme = mkTheme defaultTheme'
  where
    defaultTheme' :: SimplifiedTheme
    defaultTheme' active importance
      | isCritical importance, active = (ColorRGB 0 0 0, Just $ ColorRGB 1 0 0)
      | isCritical importance         = (ColorRGB 0.8 0.15 0.15, Nothing)
      | isError importance, active    = (ColorRGB 1 0.3 0, Nothing)
      | isError importance            = (ColorRGB 0.7 0.35 0.2, Nothing)
      | isWarning importance,active   = (ColorRGB 1 0.9 0, Nothing)
      | isWarning importance          = (ColorRGB 0.6 0.6 0, Nothing)
      | otherwise, active             = (ColorRGB 1 1 1, Nothing)
      | otherwise                     = (ColorRGB (0x96/255) (0x98/255) (0x96/255), Nothing)
