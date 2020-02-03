module QBar.Theme where

import QBar.BlockOutput

import Control.Lens ((^.))
import Control.Monad.State.Lazy (State, evalState, get, put)
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV (hsv)
import qualified Data.Text.Lazy as T
import Numeric (showHex)


data Color = ColorRGB (RGB Double) | ColorRGBA (RGB Double) Double


type Theme = [BlockOutput] -> [(PangoText, Maybe PangoText)]
type SimplifiedTheme = Bool -> Importance -> (Color, Maybe Color)
type AnimatedTheme = Double -> Theme


mkTheme :: SimplifiedTheme -> Theme
mkTheme theming' = map themeBlock
  where
    themeBlock :: BlockOutput -> (PangoText, Maybe PangoText)
    themeBlock block = (fullText', shortText')
      where
        theming :: SimplifiedTheme
        theming
          | block ^. invalid = invalidSimplifiedTheme
          | otherwise = theming'
        fullText' :: PangoText
        fullText' = themeBlockText theming $ block ^. fullText
        shortText' :: Maybe PangoText
        shortText' = themeBlockText theming <$> block ^. shortText
    themeBlockText :: SimplifiedTheme -> BlockText -> PangoText
    themeBlockText theming (BlockText b) = foldMap (themeSegment theming) b
    themeSegment :: SimplifiedTheme -> BlockTextSegment -> PangoText
    themeSegment theming BlockTextSegment {active, importance, text} = (coloredText' $ theming active importance) text


invalidColor :: Color
invalidColor = ColorRGBA (RGB (0x96 / 255) (0x98 / 255) (0x96 / 255)) (0x77 / 255)


invalidSimplifiedTheme :: SimplifiedTheme
invalidSimplifiedTheme _ _ = (invalidColor, Nothing)


invalidTheme :: Theme
invalidTheme = mkTheme invalidSimplifiedTheme


defaultTheme :: Theme
defaultTheme = mkTheme defaultTheme'
  where
    defaultTheme' :: SimplifiedTheme
    defaultTheme' active importance
      | isCritical importance, active = (ColorRGB (RGB 0 0 0), Just $ ColorRGB (RGB 1 0 0))
      | isCritical importance         = (ColorRGB (RGB 0.8 0.15 0.15), Nothing)
      | isError importance, active    = (ColorRGB (RGB 1 0.3 0), Nothing)
      | isError importance            = (ColorRGB (RGB 0.7 0.35 0.2), Nothing)
      | isWarning importance,active   = (ColorRGB (RGB 1 0.9 0), Nothing)
      | isWarning importance          = (ColorRGB (RGB 0.6 0.6 0), Nothing)
      | otherwise, active             = (ColorRGB (RGB 1 1 1), Nothing)
      | otherwise                     = (ColorRGB (RGB (0x96 / 255) (0x98 / 255) (0x96 / 255)), Nothing)


rainbowTheme :: Double -> Theme
rainbowTheme time blocks = reverse $ evalState (mapM rainbowBlock $ reverse blocks) 0
  where
    rainbowBlock :: BlockOutput -> State Integer (PangoText, Maybe PangoText)
    rainbowBlock block = do
      let text = rawText $ block ^. fullText
      let chars = T.unpack . T.reverse $ text
      coloredChars <- mapM rainbowChar chars
      let rainbowText = T.concat . reverse $ coloredChars
      return (rainbowText, Nothing)
    rainbowChar :: Char -> State Integer T.Text
    rainbowChar char = do
      color <- nextRainbowColor
      return $ coloredText color $ T.singleton char
    nextRainbowColor :: State Integer Color
    -- nextRainbowColor = state $ \index -> (rainbowColor (fromInteger index), index + 1)
    nextRainbowColor = do
      index <- get
      put $ index + 1
      return $ rainbowColor (fromInteger index + time * 10)
    rainbowColor :: Double -> Color
    rainbowColor position =
      let hue' = position * 3
          color = hsv hue' 0.8 1.0
      in ColorRGB color


coloredText :: Color -> T.Text -> PangoText
coloredText color text = "<span color='" <> pangoColor color <> "'>" <> text <> "</span>"

coloredText' :: (Color, Maybe Color) -> T.Text -> PangoText
coloredText' (foreground, Nothing) text = "<span color='" <> pangoColor foreground <> "'>" <> text <> "</span>"
coloredText' (foreground, Just background) text = "<span color='" <> pangoColor foreground <> "' background='" <> pangoColor background <> "'>" <> text <> "</span>"


pangoColor :: Color -> T.Text
pangoColor = pangoColor'
  where
    pangoColor' :: Color -> T.Text
    pangoColor' (ColorRGB rgb) = pangoRGB rgb
    pangoColor' (ColorRGBA rgb a) = pangoRGB rgb <> hexColorComponent a

    pangoRGB :: RGB Double -> T.Text
    pangoRGB (RGB r g b) =
      let r' = hexColorComponent r
          g' = hexColorComponent g
          b' = hexColorComponent b
      in "#" <> r' <> g' <> b'
    hexColorComponent :: Double -> T.Text
    hexColorComponent val = paddedHexComponent $ T.pack $ showHex (max 0 $ min 255 (truncate (val * 255) :: Int)) ""
    paddedHexComponent :: T.Text -> T.Text
    paddedHexComponent hex =
      let len = 2 - T.length hex
          padding = if len == 1 then "0" else ""
      in padding <> hex
