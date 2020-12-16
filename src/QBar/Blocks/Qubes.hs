module QBar.Blocks.Qubes where

import QBar.BlockHelper
import QBar.BlockOutput
import QBar.Core
import QBar.Qubes.AdminAPI (qubesUsageOfDefaultPool, qubesMonitorProperty, qubesGetProperty, qubesEvents, QubesPropertyInfo (..))

import Control.Concurrent.Async
import Control.Monad.Reader (runReaderT)
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

pipeBlockWithEvents :: forall a. Producer a BarIO ExitBlock -> (Either BlockEvent a -> BarIO (Maybe BlockOutput)) -> Block
pipeBlockWithEvents prod block = do
  bar <- askBar
  (output, input) <- liftIO $ spawn $ newest 1
  exitValue <- forkBarEffect bar $ prod >-> P.map Right >-> forever (toOutput output)
  fromInput input >-> forever (update output)
  liftIO $ wait exitValue
  where
  forkBarEffect :: MonadIO m => Bar -> Effect BarIO b -> m (Async b)
  forkBarEffect bar = liftIO . async . flip runReaderT bar . P.runSafeT . runEffect

  forkEffect :: MonadIO m => Effect IO () -> m ()
  forkEffect = void . liftIO . forkIO . runEffect

  update :: Output (Either (BlockOutput, BlockEvent) a) -> Pipe (Either (BlockOutput, BlockEvent) a) (BlockState, BlockUpdateReason) BarIO ()
  update output = await >>= \case
    Right prop -> update' $ Right prop
    Left (blockOutput, event) -> do
      let state = Just (blockOutput, Nothing)
      yield (invalidateBlockState state, EventUpdate)
      update' $ Left event
    where
    update' :: Either BlockEvent a -> P.Pipe b BlockUpdate BarIO ()
    update' prop = do
      Just blockOutput <- lift $ block prop
      pushBlockUpdate' (handleClick blockOutput) blockOutput

    handleClick blockOutput event = do
      forkEffect $ yield (Left (blockOutput, event)) >-> toOutput output

qubesMonitorPropertyBlock :: BL.ByteString -> Block
qubesMonitorPropertyBlock name = pipeBlockWithEvents (qubesMonitorProperty qubesEvents name >> exitBlock) handle
  where
    handle = handle' <=< either (const $ liftIO $ qubesGetProperty name) return
    handle' QubesPropertyInfo {propValue, propIsDefault} = return $ Just $ mkBlockOutput $ normalText $ decode propValue <> (if propIsDefault then " (D)" else "")
    decode = decodeUtf8With lenientDecode
