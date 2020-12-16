module QBar.Blocks.Qubes where

import QBar.BlockHelper
import QBar.BlockOutput
import QBar.Core
import QBar.Qubes.AdminAPI (qubesUsageOfDefaultPool, qubesMonitorProperty, qubesEvents, QubesPropertyInfo (..))

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Pipes

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
    chooseColor _free = normalText  --TODO
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
qubesMonitorPropertyBlock name = fmap (const ExitBlock) (qubesMonitorProperty qubesEvents name) >-> forever update
  where
  update = do
    QubesPropertyInfo {propValue, propIsDefault} <- await
    pushBlockUpdate' handleClick $ mkBlockOutput $ normalText $ decode propValue <> (if propIsDefault then " (D)" else "")
  handleClick _ = return ()  --TODO
  decode = decodeUtf8With lenientDecode
