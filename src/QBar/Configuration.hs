{-# LANGUAGE TemplateHaskell #-}

module QBar.Configuration where

import QBar.Blocks
import QBar.Core

import Data.Aeson.TH
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as T
import Control.Monad.Reader
import Pipes

data BarConfiguration = BarConfiguration {
  intervalSeconds :: Maybe Int,
  blocks :: [BlockConfiguration]
}

data BlockConfiguration = Modify ModifyConfiguration
  | Date
  | ExternalCommand ExternalCommandConfiguration

data ModifyConfiguration = ModifyConfiguration {
  block :: BlockConfiguration,
  enableAutoPadding :: Maybe Bool,
  icon :: Maybe T.Text
}

data ExternalCommandConfiguration = ExternalCommandConfiguration {
  command :: FilePath,
  persistent :: Maybe Bool
}

$(deriveJSON defaultOptions ''BarConfiguration)
$(deriveJSON defaultOptions ''BlockConfiguration)
$(deriveJSON defaultOptions ''ModifyConfiguration)
$(deriveJSON defaultOptions ''ExternalCommandConfiguration)


type ConfigurationM = Reader (PullBlock -> CachedBlock)

cachePullBlock :: PullBlock -> ConfigurationM CachedBlock
cachePullBlock pullBlock = ask <*> return pullBlock



applyBarConfiguration :: BarConfiguration -> BarIO ()
applyBarConfiguration barConfiguration@BarConfiguration{ intervalSeconds } = do
  cachePullBlock' <- sharedInterval $ fromMaybe 10 intervalSeconds
  let blocks' = runReader (evaluateBarConfiguration barConfiguration) cachePullBlock'
  mapM_ addBlock blocks'

evaluateBarConfiguration :: BarConfiguration -> ConfigurationM [CachedBlock]
evaluateBarConfiguration BarConfiguration { blocks } = mapM evaluateBlockConfiguration blocks


evaluateBlockConfiguration :: BlockConfiguration -> ConfigurationM CachedBlock

evaluateBlockConfiguration (Modify ModifyConfiguration { enableAutoPadding, icon, block }) = do
  block' <- evaluateBlockConfiguration block
  let block'' = case icon of
        Just icon' -> block' >-> modify (addIcon icon')
        Nothing -> block'
  let block''' = if enableAutoPadding == Just True
      then block'' >-> autoPadding
      else block''
  return block'''

evaluateBlockConfiguration Date = return $ toCachedBlock dateBlock

evaluateBlockConfiguration (ExternalCommand ExternalCommandConfiguration { command, persistent }) = if fromMaybe False persistent
  then return $ startPersistentBlockScript command
  else cachePullBlock $ blockScript command
