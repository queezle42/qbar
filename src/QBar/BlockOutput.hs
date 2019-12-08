module BlockOutput where

import qualified Data.Text.Lazy as T

newtype BlockText = BlockText [BlockTextSegment]
instance Semigroup BlockText where
  (BlockText a) <> (BlockText b) = BlockText (a <> b)
instance Monoid BlockText where
  mempty = BlockText []

data BlockTextSegment = BlockTextSegment {
    active :: Bool,
    importance :: Importance,
    text :: T.Text
  }
  | PangoTextSegment T.Text

type Importance = Float

mkText :: Bool -> Importance -> T.Text -> BlockText
mkText active importance text = BlockText [BlockTextSegment { text, active, importance }]

activeImportantText :: Importance -> T.Text -> BlockText
activeImportantText = mkText True

importantText :: Importance -> T.Text -> BlockText
importantText = mkText False

activeText :: T.Text -> BlockText
activeText = mkText True 0

normalText :: T.Text -> BlockText
normalText = mkText False 0

pangoText :: T.Text -> BlockText
pangoText pango = BlockText [PangoTextSegment pango]

surroundWith :: (T.Text -> BlockText) -> T.Text -> T.Text -> BlockText -> BlockText
surroundWith format left right middle = (format left) <> middle <> (format right)
