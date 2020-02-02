{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module QBar.BlockOutput where

import QBar.Pango

import Control.Lens
import Data.Aeson.TH
import Data.Int (Int64)
import qualified Data.Text.Lazy as T


data BlockOutput = BlockOutput {
    _fullText :: BlockText,
    _shortText :: Maybe BlockText,
    _blockName :: Maybe T.Text,
    _invalid :: Bool
  }
  deriving (Eq, Show)


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
  | PangoTextSegment PangoText
  deriving (Eq, Show)

type PangoText = T.Text

type Importance = Float

$(deriveJSON defaultOptions ''BlockOutput)
makeLenses ''BlockOutput
$(deriveJSON defaultOptions ''BlockTextSegment)
$(deriveJSON defaultOptions ''BlockText)


mkBlockOutput :: BlockText -> BlockOutput
mkBlockOutput text = BlockOutput {
  _fullText = text,
  _shortText = Nothing,
  _blockName = Nothing,
  _invalid = False
}

mkErrorOutput :: T.Text -> BlockOutput
mkErrorOutput = mkBlockOutput . importantText criticalImportant

emptyBlock :: BlockOutput
emptyBlock = mkBlockOutput mempty

addIcon :: T.Text -> BlockOutput -> BlockOutput
addIcon icon = over fullText $ (<>) . normalText $ icon <> " "


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
toImportance (tMax, tCritical, tError, tWarning, tNormal, tMinimal) =
  toImportance' (Just tMax, tCritical, tError, tWarning, tNormal, Just tMinimal)

toImportance' :: forall a. Real a => (Maybe a, a, a, a, a, Maybe a) -> a -> Importance
toImportance' (tMax, tCritical, tError, tWarning, tNormal, tMinimal) val
  | tCritical <= val = 4 + valueCritical tMax  tCritical val
  | tError  <= val = 3 + linearMatch tCritical tError  val
  | tWarning <= val = 2 + linearMatch tError  tWarning val
  | tNormal <= val = 1 + linearMatch tWarning tNormal val
  | otherwise = 0 + valueOtherwise tNormal tMinimal  val
  where
    e :: Importance
    e = exp 1
    linearMatch :: a -> a -> a -> Importance
    linearMatch u l v = frac (v - l) (u - l)
    logarithmicMatch :: a -> a -> Importance
    logarithmicMatch l u = 1 - 1 / log (e + realToFrac (u - l))
    frac :: a -> a -> Importance
    frac a b = realToFrac a / realToFrac b
    valueCritical :: Maybe a -> a -> a -> Importance
    valueCritical (Just tMax') tCritical' v
      | tMax' > v = linearMatch tMax' tCritical' v
      | otherwise = 1
    valueCritical Nothing tCritical' v = logarithmicMatch tCritical' v
    valueOtherwise :: a -> Maybe a -> a -> Importance
    valueOtherwise tNormal' (Just tMinimal') v
      | tMinimal' < v = linearMatch tNormal' tMinimal' v
      | otherwise = 0
    valueOtherwise tNormal' Nothing v = 1 - logarithmicMatch v tNormal'


invalidateBlock :: BlockOutput -> BlockOutput
invalidateBlock block@BlockOutput{ _fullText, _shortText } = block {
  _fullText = normalText . rawText $ _fullText,
  _shortText = normalText . rawText <$> _shortText,
  _invalid = True
}


rawText :: BlockText -> T.Text
rawText (BlockText b) = foldMap rawTextFromSegment b
  where
    rawTextFromSegment :: BlockTextSegment -> T.Text
    rawTextFromSegment BlockTextSegment{text} = text
    rawTextFromSegment (PangoTextSegment text) =
      case parsePango text of
        Left _ -> text
        Right parsed -> removeFormatting parsed

printedLength :: BlockText -> Int64
printedLength (BlockText b) = sum . map segmentLength $ b
  where
    segmentLength :: BlockTextSegment -> Int64
    segmentLength BlockTextSegment { text } = T.length text
    segmentLength (PangoTextSegment pango) = either (const $ T.length pango) (T.length . removeFormatting) $ parsePango pango

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

pangoText :: PangoText -> BlockText
pangoText pango = BlockText [PangoTextSegment pango]

surroundWith :: (T.Text -> BlockText) -> T.Text -> T.Text -> BlockText -> BlockText
surroundWith format left right middle = format left <> middle <> format right