module QBar.Theme (
  Theme(..),
  ThemedBlockOutput(..),
  ThemedBlockText(..),
  ThemedBlockTextSegment(..),
  defaultTheme,
  findTheme,
  isAnimated,
  mkTheme,
  mkThemedBlockOutput,
  themeNames,
  themes,
  whiteThemedBlockOutput,
) where

import QBar.BlockOutput
import QBar.Color
import QBar.Prelude

import Control.Applicative ((<|>))
import Control.Lens ((^.))
import Control.Monad.State.Lazy (State, evalState, get, put)
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV (hsv)
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Pipes


data ThemedBlockOutput = ThemedBlockOutput {
    _fullText :: ThemedBlockText,
    _shortText :: Maybe ThemedBlockText,
    _blockName :: Maybe T.Text
  }
  deriving (Eq, Show)

newtype ThemedBlockText = ThemedBlockText [ThemedBlockTextSegment]
  deriving (Eq, Show)
instance Semigroup ThemedBlockText where
  (ThemedBlockText a) <> (ThemedBlockText b) = ThemedBlockText (a <> b)
instance Monoid ThemedBlockText where
  mempty = ThemedBlockText []

data ThemedBlockTextSegment = ThemedBlockTextSegment {
    themedSegmentText :: T.Text,
    color :: Color,
    backgroundColor :: Maybe Color
  }
  deriving (Eq, Show)


data Theme = StaticTheme StaticTheme | AnimatedTheme AnimatedTheme

type StaticTheme = [BlockOutput] -> [ThemedBlockOutput]
type SimplifiedTheme = Bool -> Importance -> (Color, Maybe Color)
type AnimatedTheme = forall r. Pipe [BlockOutput] [ThemedBlockOutput] IO r

isAnimated :: Theme -> Bool
isAnimated (AnimatedTheme _) = True
isAnimated _ = False


themesList :: [(Text, Theme)]
themesList = [
    ("default", defaultTheme),
    ("rainbow", rainbowTheme)
  ]

themeNames :: [Text]
themeNames = map fst themesList

themes :: HM.HashMap Text Theme
themes = HM.fromList themesList


findTheme :: Text -> Either Text Theme
findTheme themeName = maybe invalidThemeName Right $ HM.lookup themeName themes
  where
    invalidThemeName = Left $ "Invalid theme: " <> themeName

mkTheme :: SimplifiedTheme -> Theme
mkTheme theming' = StaticTheme $ map themeBlock
  where
    themeBlock :: BlockOutput -> ThemedBlockOutput
    themeBlock block@BlockOutput{_blockName} = ThemedBlockOutput{_fullText = fullText', _shortText = shortText', _blockName}
      where
        theming :: SimplifiedTheme
        theming
          | block ^. invalid = invalidSimplifiedTheme
          | otherwise = theming'
        fullText' :: ThemedBlockText
        fullText' = themeBlockText theming $ block ^. fullText
        shortText' :: Maybe ThemedBlockText
        shortText' = themeBlockText theming <$> block ^. shortText
    themeBlockText :: SimplifiedTheme -> BlockText -> ThemedBlockText
    themeBlockText theming (BlockText b) = ThemedBlockText $ themeSegment theming <$> b
    themeSegment :: SimplifiedTheme -> BlockTextSegment -> ThemedBlockTextSegment
    themeSegment theming BlockTextSegment {active, importance, segmentText} = mkThemedSegment (theming active importance) segmentText
    themeSegment theming StyledBlockTextSegment {color, backgroundColor, segmentText} = mkThemedSegment (themedColor, themedBackgroundColor) segmentText
      where
        themedColor :: Color
        themedColor = fromMaybe normalThemedColor color
        themedBackgroundColor :: Maybe Color
        themedBackgroundColor = backgroundColor <|> normalThemedBackground
        (normalThemedColor, normalThemedBackground) = theming False normalImportant

mkThemedBlockOutput :: (Color, Maybe Color) -> Text -> ThemedBlockOutput
mkThemedBlockOutput color text = ThemedBlockOutput {
  _fullText = mkThemedText color text,
  _shortText = Nothing,
  _blockName = Nothing
}

mkThemedText :: (Color, Maybe Color) -> Text -> ThemedBlockText
mkThemedText color text = ThemedBlockText [mkThemedSegment color text]

mkThemedSegment :: (Color, Maybe Color) -> Text -> ThemedBlockTextSegment
mkThemedSegment (color, backgroundColor) text = ThemedBlockTextSegment{themedSegmentText=text, color, backgroundColor}

whiteThemedBlockOutput :: Text -> ThemedBlockOutput
whiteThemedBlockOutput = mkThemedBlockOutput (ColorRGB (RGB 1 1 1), Nothing)


invalidColor :: Color
invalidColor = ColorRGBA (RGB (0x96 / 255) (0x98 / 255) (0x96 / 255)) (0x77 / 255)

invalidSimplifiedTheme :: SimplifiedTheme
invalidSimplifiedTheme _ _ = (invalidColor, Nothing)

defaultTheme :: Theme
defaultTheme = mkTheme defaultTheme'
  where
    defaultTheme' :: SimplifiedTheme
    defaultTheme' True (CriticalImportant _) = (ColorRGB (RGB 0 0 0), Just $ ColorRGB (RGB 1 0 0))
    defaultTheme' False (CriticalImportant _) = (ColorRGB (RGB 0.8 0.15 0.15), Nothing)
    defaultTheme' True (ErrorImportant _) = (ColorRGB (RGB 1 0.3 0), Nothing)
    defaultTheme' False (ErrorImportant _) = (ColorRGB (RGB 0.7 0.35 0.2), Nothing)
    defaultTheme' True (WarnImportant _) = (ColorRGB (RGB 1 0.9 0), Nothing)
    defaultTheme' False (WarnImportant _) = (ColorRGB (RGB 0.6 0.6 0), Nothing)
    defaultTheme' True (NormalImportant _) = (ColorRGB (RGB 1 1 1), Nothing)
    defaultTheme' False (NormalImportant _) = (ColorRGB (RGB (0x96 / 255) (0x98 / 255) (0x96 / 255)), Nothing)


rainbowTheme :: Theme
rainbowTheme = AnimatedTheme rainbowThemePipe
  where
    rainbowThemePipe :: AnimatedTheme
    rainbowThemePipe = do
      time <- liftIO $ fromRational . toRational <$> getPOSIXTime
      yield . rainbowThemePipe' time =<< await
      rainbowThemePipe
    rainbowThemePipe' :: Double -> StaticTheme
    rainbowThemePipe' time blocks = reverse $ evalState (mapM rainbowBlock $ reverse blocks) 0
      where
        rainbowBlock :: BlockOutput -> State Integer ThemedBlockOutput
        rainbowBlock block@BlockOutput{_blockName} = do
          let text = rawText $ block ^. fullText
          let chars = T.unpack . T.reverse $ text
          coloredChars <- mapM rainbowChar chars
          let rainbowText = reverse $ coloredChars
          return $ ThemedBlockOutput {
            _blockName,
            _fullText = ThemedBlockText rainbowText,
            _shortText = Nothing
          }
        rainbowChar :: Char -> State Integer ThemedBlockTextSegment
        rainbowChar char = do
          color <- nextRainbowColor
          return $ mkThemedSegment (color, Nothing) $ T.singleton char
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
