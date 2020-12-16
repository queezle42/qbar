module QBar.Blocks.Qubes where

import QBar.BlockHelper
import QBar.BlockOutput
import QBar.Core
import QBar.Qubes.AdminAPI (qubesUsageOfDefaultPool, qubesMonitorProperty, qubesGetProperty, qubesEvents, QubesPropertyInfo (..))

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Pipes as P
import Pipes.Concurrent as P
import qualified Pipes.Prelude as P
import qualified Pipes.Safe as P

diskIcon :: T.Text
diskIcon = "💾\xFE0E"

diskUsageQubesBlock :: Block
diskUsageQubesBlock = runPollBlock $ forever $ do
  output <- liftBarIO action
  yieldBlockUpdate $ addIcon diskIcon output 
  where
    action :: BarIO BlockOutput
    action = liftIO qubesUsageOfDefaultPool >>= \case
      (Just usage, Just size) -> return $ createBlockOutput $ size - usage
      _ -> return $ mkErrorOutput "unknown"
    createBlockOutput :: Int -> BlockOutput
    createBlockOutput free =
      mkBlockOutput $ chooseColor free $ formatSize free
    chooseColor free = if free < 40 * 1024*1024*1024
      then activeText
      else normalText
    sizeUnits = [
        ("T", 1024*1024*1024*1024),
        ("G", 1024*1024*1024),
        ("M", 1024*1024),
        ("k", 1024),
        (" bytes", 1)
      ]
    formatSize size = case filter ((<size) . snd) sizeUnits of
      ((unit, factor) : _) -> T.pack (show $ size `div` factor) <> unit
      _ -> T.pack (show size) <> " bytes"

qubesMonitorPropertyBlock :: BL.ByteString -> Block
qubesMonitorPropertyBlock name = do
  (output, input) <- liftIO $ spawn $ newest 1
  forkSafeEffect $ qubesMonitorProperty qubesEvents name >-> P.map Right >-> toOutput output
  toExitBlock $ fromInput input >-> forever (update output)
  where
  forkSafeEffect :: MonadIO m => Effect (P.SafeT IO) () -> m ()
  forkSafeEffect = void . liftIO . forkIO . P.runSafeT . runEffect

  forkEffect :: MonadIO m => Effect IO () -> m ()
  forkEffect = void . liftIO . forkIO . runEffect

  toExitBlock = fmap (const ExitBlock) 

  decode = decodeUtf8With lenientDecode

  update output = await >>= \case
    Right prop -> update' prop
    Left blockOutput -> do
      let state = Just (blockOutput, Nothing)
      yield (invalidateBlockState state, EventUpdate)
      prop <- liftIO (qubesGetProperty name)
      update' prop
    where
    update' prop = do
      let QubesPropertyInfo {propValue, propIsDefault} = prop
      let blockOutput = mkBlockOutput $ normalText $ decode propValue <> (if propIsDefault then " (D)" else "")
      pushBlockUpdate' (handleClick blockOutput) blockOutput

    handleClick blockOutput _ = do
      forkEffect $ yield (Left blockOutput) >-> toOutput output
