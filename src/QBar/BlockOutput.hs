{-# LANGUAGE TemplateHaskell #-}

module QBar.BlockOutput where

import QBar.BlockText

import Control.Lens
import Data.Aeson.TH
import qualified Data.Text.Lazy as T

data BlockOutput = BlockOutput
  { _fullText :: BlockText
  , _shortText :: Maybe BlockText
  , _blockName :: Maybe T.Text
  , _invalid :: Bool
  }
  deriving (Eq, Show)
$(deriveJSON defaultOptions ''BlockOutput)
makeLenses ''BlockOutput


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

