module QBar.Blocks.Qubes (
  diskUsageQubesBlock,
  qubesMonitorPropertyBlock,
  qubesVMCountBlock,
) where

import QBar.BlockHelper
import QBar.BlockOutput
import QBar.Core
import QBar.Prelude
import QBar.Qubes.AdminAPI (qubesUsageOfDefaultPool, qubesMonitorProperty, qubesGetProperty, qubesEvents, QubesPropertyInfo (..), qubesListVMs, qubesListVMsP, QubesVMState (..), vmState)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Pipes as P
import Pipes.Core as P

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
    formatSize size = case filter ((< size) . snd) sizeUnits of
      ((unit, factor) : _) -> T.pack (show $ size `div` factor) <> unit
      _ -> T.pack (show size) <> " bytes"

pipeBlockWithEvents :: forall a. Producer a BarIO () -> (Either BlockEvent a -> BarIO (Maybe BlockOutput)) -> Block
pipeBlockWithEvents prod block = runSignalBlock Nothing (Just produce) sblock
  where
    produce :: (a -> IO ()) -> BarIO ()
    produce yield' = runEffect $ prod >-> forever (await >>= liftIO . yield')
    sblock :: Signal a -> P.Server (Signal a) (Maybe BlockOutput) BarIO ExitBlock
    sblock = lift . sblock' >=> respond >=> sblock
    sblock' :: Signal a -> BarIO (Maybe BlockOutput)
    sblock' RegularSignal = return Nothing  -- ignore timer
    sblock' (UserSignal x) = block $ Right x
    sblock' (EventSignal x) = block $ Left x

qubesMonitorPropertyBlock :: BL.ByteString -> Block
qubesMonitorPropertyBlock name = pipeBlockWithEvents (qubesMonitorProperty qubesEvents name) handle
  where
    handle = fmap handle' . either (const $ liftIO $ qubesGetProperty name) return
    handle' QubesPropertyInfo {propValue, propIsDefault} = Just $ mkBlockOutput $ normalText $ decode propValue <> (if propIsDefault then " (D)" else "")
    decode = decodeUtf8With lenientDecode

qubesVMCountBlock :: Block
qubesVMCountBlock = pipeBlockWithEvents qubesListVMsP $ fmap countVMs . either (const $ liftIO $ qubesListVMs) return where
  countVMs = Just . format . M.size . M.filterWithKey isRunningVM
  isRunningVM name x = vmState x == VMRunning && name /= "dom0"
  format n = mkBlockOutput $ normalText $ T.pack (show n) <> " Qube" <> (if n /= 1 then "s" else "")
