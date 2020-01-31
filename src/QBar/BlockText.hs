{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module QBar.BlockText where

import Data.Aeson.TH
import qualified Data.Text.Lazy as T
import Data.Int (Int64)
import QBar.Pango

newtype BlockText = BlockText [BlockTextSegment]
  deriving (Eq, Show)
instance Semigroup BlockText where
  (BlockText a) <> (BlockText b) = BlockText (a <> b)
instance Monoid BlockText where
  mempty = BlockText []

intercalate :: Monoid a => a -> [a] -> a
intercalate _ [] = mempty
intercalate _ [x] = x
intercalate inter (x:xs) = x <> inter <> intercalate inter xs

data BlockTextSegment = BlockTextSegment {
    active :: Bool,
    importance :: Importance,
    text :: T.Text
  }
  | PangoTextSegment T.Text
  deriving (Eq, Show)

type Importance = Float

$(deriveJSON defaultOptions ''BlockTextSegment)
$(deriveJSON defaultOptions ''BlockText)


normalImportant :: Importance
normalImportant = 1
warnImportant :: Importance
warnImportant = 2
errorImportant :: Importance
errorImportant = 3
criticalImportant :: Importance
criticalImportant = 4

isCritical :: Importance -> Bool
isCritical i
  | i >= criticalImportant = True
  | otherwise = False
isError :: Importance -> Bool
isError i
  | isCritical i = False
  | i >= errorImportant = True
  | otherwise = False
isWarning :: Importance -> Bool
isWarning i
  | isCritical i = False
  | isError i = False
  | i >= warnImportant = True
  | otherwise = False
isNormal :: Importance -> Bool
isNormal i
  | isCritical i = False
  | isError i = False
  | isWarning i = False
  | otherwise = True

toImportance :: Real a => (a, a, a, a, a, a) -> a -> Importance
toImportance (tMax, tCrit, tErr, tWarn, tNorm, tMin) =
  toImportance' (Just tMax, tCrit, tErr, tWarn, tNorm, Just tMin)

toImportance' :: forall a. Real a => (Maybe a, a, a, a, a, Maybe a) -> a -> Importance
toImportance' (tMax, tCrit, tErr, tWarn, tNorm, tMin) val
  | tCrit <= val = 4 + valueCrit      tMax  tCrit val
  | tErr  <= val = 3 + linearMatch    tCrit tErr  val
  | tWarn <= val = 2 + linearMatch    tErr  tWarn val
  | tNorm <= val = 1 + linearMatch    tWarn tNorm val
  | otherwise    = 0 + valueOtherwise tNorm tMin  val
  where
    e :: Importance
    e = exp 1
    linearMatch :: a -> a -> a -> Importance
    linearMatch u l v = frac (v - l) (u - l)
    logarithmicMatch :: a -> a -> Importance
    logarithmicMatch l u = 1 - 1 / log (e + realToFrac (u - l))
    frac :: a -> a -> Importance
    frac a b = realToFrac a / realToFrac b
    valueCrit :: Maybe a -> a -> a -> Importance
    valueCrit (Just tMax') tCrit' v
      | tMax' > v = linearMatch tMax' tCrit' v
      | otherwise = 1
    valueCrit Nothing tCrit' v = logarithmicMatch tCrit' v
    valueOtherwise :: a -> Maybe a -> a -> Importance
    valueOtherwise tNorm' (Just tMin') v
      | tMin' < v = linearMatch tNorm' tMin' v
      | otherwise = 0
    valueOtherwise tNorm' Nothing v = 1 - logarithmicMatch v tNorm'


removePango :: BlockText -> T.Text
removePango (BlockText b) = foldr ((<>) . removePangoFromSegment) "" b
  where
    removePangoFromSegment :: BlockTextSegment -> T.Text
    removePangoFromSegment BlockTextSegment { active=_active, importance=_importance, text } = text
    removePangoFromSegment (PangoTextSegment text) =
      case parsePango text of
        Left _ -> text
        Right parsed -> removeFormatting parsed

printedLength :: BlockText -> Int64
printedLength (BlockText b) = foldr ((+) . printedLength') 0 b
  where
    printedLength' :: BlockTextSegment -> Int64
    printedLength' BlockTextSegment { text, active=_, importance=_ } = T.length text
    printedLength' (PangoTextSegment _) = 0

mkText :: Bool -> Importance -> T.Text -> BlockText
mkText active importance text = BlockText [BlockTextSegment { text = pangoFriendly text, active, importance }]
  where
    pangoFriendly :: T.Text -> T.Text
    pangoFriendly = T.replace "<" "&lt;" . T.replace ">" "&gt;" . T.replace "&" "&amp;"

activeImportantText :: Importance -> T.Text -> BlockText
activeImportantText = mkText True

importantText :: Importance -> T.Text -> BlockText
importantText = mkText False

activeText :: T.Text -> BlockText
activeText = mkText True normalImportant

normalText :: T.Text -> BlockText
normalText = mkText False normalImportant

pangoText :: T.Text -> BlockText
pangoText pango = BlockText [PangoTextSegment pango]

surroundWith :: (T.Text -> BlockText) -> T.Text -> T.Text -> BlockText -> BlockText
surroundWith format left right middle = format left <> middle <> format right

data Color = ColorRGB Float Float Float | ColorRGBA Float Float Float Float
colorToHex :: Color -> T.Text
colorToHex = colorToHex'
  where
    colorToHex' :: Color -> T.Text
    colorToHex' (ColorRGB r g b) = "#" <> (toDualHex . floor) (r * 255) <> (toDualHex . floor) (g * 255)  <> (toDualHex . floor) (b * 255)
    colorToHex' (ColorRGBA r g b a) = "#" <> (toDualHex . floor) (r * 255) <> (toDualHex . floor) (g * 255)  <> (toDualHex . floor) (b * 255) <> (toDualHex . floor) (a * 255)
    toHex :: Int -> T.Text
    toHex 0 = "0"
    toHex 1 = "1"
    toHex 2 = "2"
    toHex 3 = "3"
    toHex 4 = "4"
    toHex 5 = "5"
    toHex 6 = "6"
    toHex 7 = "7"
    toHex 8 = "8"
    toHex 9 = "9"
    toHex 10 = "A"
    toHex 11 = "B"
    toHex 12 = "C"
    toHex 13 = "D"
    toHex 14 = "E"
    toHex 15 = "F"
    toHex x = toHex $ mod x 16
    toDualHex :: Int -> T.Text
    toDualHex x = toHex (div x 16) <> toHex x
