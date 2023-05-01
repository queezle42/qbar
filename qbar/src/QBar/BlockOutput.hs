{-# LANGUAGE TemplateHaskell #-}

module QBar.BlockOutput where

import QBar.Color
import QBar.Prelude

import Control.Lens
import Data.Aeson
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

data BlockTextSegment = BlockTextSegment {
    active :: Bool,
    importance :: Importance,
    segmentText :: T.Text
  }
  | StyledBlockTextSegment {
    segmentText :: T.Text,
    color :: Maybe Color,
    backgroundColor :: Maybe Color
  }
  deriving (Eq, Show)

data Importance = NormalImportant Float | WarnImportant Float | ErrorImportant Float | CriticalImportant Float
  deriving (Eq, Show)


$(deriveJSON defaultOptions ''BlockOutput)
makeLenses ''BlockOutput
$(deriveJSON defaultOptions ''Importance)
$(deriveJSON defaultOptions ''BlockTextSegment)
$(deriveJSON defaultOptions ''BlockText)


mkBlockOutput :: BlockText -> BlockOutput
mkBlockOutput text = BlockOutput {
  _fullText = text,
  _shortText = Nothing,
  _blockName = Nothing,
  _invalid = False
}

mkBlockOutput' :: BlockText -> BlockText -> BlockOutput
mkBlockOutput' full short = BlockOutput {
  _fullText = full,
  _shortText = Just short,
  _blockName = Nothing,
  _invalid = False
}

mkErrorOutput :: T.Text -> BlockOutput
mkErrorOutput errorText = mkBlockOutput . importantText criticalImportant $ "[" <> errorText <> "]"

emptyBlock :: BlockOutput
emptyBlock = mkBlockOutput mempty

addIcon :: T.Text -> BlockOutput -> BlockOutput
addIcon icon = over fullText $ (<>) . normalText $ icon <> " "


normalImportant :: Importance
normalImportant = NormalImportant 0.5

normalImportant' :: Float -> Importance
normalImportant' = NormalImportant . min 1 . max 0

warnImportant :: Importance
warnImportant = WarnImportant 0.5

warnImportant' :: Float -> Importance
warnImportant' = WarnImportant . min 1 . max 0

errorImportant :: Importance
errorImportant = ErrorImportant 0.5

errorImportant' :: Float -> Importance
errorImportant' = ErrorImportant . min 1 . max 0

criticalImportant :: Importance
criticalImportant = CriticalImportant 0.5

criticalImportant' :: Float -> Importance
criticalImportant' = CriticalImportant . min 1 . max 0

isCritical :: Importance -> Bool
isCritical (CriticalImportant _) = True
isCritical _ = False

isError :: Importance -> Bool
isError (ErrorImportant _) = True
isError _ = False

isWarning :: Importance -> Bool
isWarning (WarnImportant _) = True
isWarning _ = False

isNormal :: Importance -> Bool
isNormal (NormalImportant _) = True
isNormal _ = False

toImportance :: Real a => (a, a, a, a, a) -> a -> Importance
toImportance (tMin, tWarning, tError, tCritical, tMax) =
  toImportance' (Just tMin, tWarning, tError, tCritical, Just tMax)

toImportance' :: Real a => (Maybe a, a, a, a, Maybe a) -> a -> Importance
toImportance' (tMin, tWarning, tError, tCritical, tMax) val
  | tCritical <= val = criticalImportant' valueCritical
  | tError <= val = errorImportant' $ linearMatch tCritical tError val
  | tWarning <= val = warnImportant' $ linearMatch tError tWarning val
  | otherwise = normalImportant' valueNormal
  where
    linearMatch :: Real a => a -> a -> a -> Float
    linearMatch u l v = realToFrac (v - l) / realToFrac (u - l)
    logarithmicMatch :: Real a => a -> a -> Float
    logarithmicMatch l u = (\x -> 1 - 1 / (1 + x)) . log . realToFrac $ u - l
    valueCritical :: Float
    valueCritical = case tMax of
      Just tMax' -> if tMax' > val then linearMatch tMax' tCritical val else 1
      Nothing -> logarithmicMatch tCritical val
    valueNormal :: Float
    valueNormal = case tMin of
      Just tMin' -> if tMin' < val then linearMatch tWarning tMin' val else 0
      Nothing -> 1 - logarithmicMatch val tWarning

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
    rawTextFromSegment BlockTextSegment{segmentText} = segmentText
    rawTextFromSegment StyledBlockTextSegment{segmentText} = segmentText

printedLength :: BlockText -> Int64
printedLength (BlockText b) = sum . map segmentLength $ b
  where
    segmentLength :: BlockTextSegment -> Int64
    segmentLength BlockTextSegment { segmentText } = T.length segmentText
    segmentLength StyledBlockTextSegment { segmentText } = T.length segmentText

mkText :: Bool -> Importance -> T.Text -> BlockText
mkText active importance segmentText = BlockText [BlockTextSegment { segmentText = pangoFriendly segmentText, active, importance }]
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

surroundWith :: (T.Text -> BlockText) -> T.Text -> T.Text -> BlockText -> BlockText
surroundWith format left right middle = format left <> middle <> format right

mkStyledText :: Maybe Color -> Maybe Color -> Text -> BlockText
mkStyledText color backgroundColor text = BlockText $ [StyledBlockTextSegment { segmentText=text, color, backgroundColor }]
