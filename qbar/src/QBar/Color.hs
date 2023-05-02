module QBar.Color (
  Color(..),
  colorParser,
  hexColorText,
) where

import QBar.Prelude

import Data.Aeson
import Data.Bits ((.|.), shiftL)
import Data.Char (ord)
import Data.Attoparsec.Text.Lazy as A
import Data.Colour.RGBSpace
import qualified Data.Text.Lazy as T
import Numeric (showHex)

data Color = ColorRGB (RGB Double) | ColorRGBA (RGB Double) Double
  deriving (Eq, Show)
instance FromJSON Color where
  parseJSON = withText "Color" $ either fail pure . parseOnly (colorParser <* endOfInput)
instance ToJSON Color where
  toJSON = String . T.toStrict . hexColorText

hexColorText :: Color -> Text
hexColorText = hexColor'
  where
    hexColor' :: Color -> Text
    hexColor' (ColorRGB rgb) = pangoRGB rgb
    hexColor' (ColorRGBA rgb a) = pangoRGB rgb <> hexColorComponent a

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


colorParser :: Parser Color
colorParser = do
  void $ char '#'
  rgb <- RGB <$> doubleFromHex2 <*> doubleFromHex2 <*> doubleFromHex2
  option (ColorRGB rgb) (ColorRGBA rgb <$> doubleFromHex2)
  where
    doubleFromHex2 :: Parser Double
    doubleFromHex2 = (/ 256) . fromIntegral <$> hexadecimal'' 2

    -- Variant of 'Data.Attoparsec.Text.hexadecimal' that parses a fixed amount of digits.
    hexadecimal'' :: Int -> A.Parser Int
    hexadecimal'' digits = foldl step 0 <$> count digits (satisfy isHexDigit)
      where
        isHexDigit c = (c >= '0' && c <= '9') ||
                      (c >= 'a' && c <= 'f') ||
                      (c >= 'A' && c <= 'F')

        step :: Int -> Char -> Int
        step a c | w >= 48 && w <= 57  = (a `shiftL` 4) .|. (w - 48)
                | w >= 97             = (a `shiftL` 4) .|. (w - 87)
                | otherwise           = (a `shiftL` 4) .|. (w - 55)
          where w = ord c
