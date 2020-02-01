{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module QBar.BlockOutput where

import QBar.Pango

import Control.Lens
import Data.Aeson.TH
import Data.Int (Int64)
import qualified Data.Text.Lazy as T


data BlockOutput = BlockOutput
  { _fullText :: BlockText
  , _shortText :: Maybe BlockText
  , _blockName :: Maybe T.Text
  , _invalid :: Bool
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
mkBlockOutput text = BlockOutput
  { _fullText = text
  , _shortText = Nothing
  , _blockName = Nothing
  , _invalid = False
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