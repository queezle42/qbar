module QBar.Blocks.DiskUsage (
  diskUsageBlock,
) where

import QBar.BlockHelper
import QBar.BlockOutput
import QBar.Core
import QBar.Prelude

import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import System.Exit
import System.Process.Typed (shell, readProcessStdout)

diskIcon :: T.Text
diskIcon = "💾\xFE0E"

diskUsageBlock :: Text -> Block
diskUsageBlock path = runPollBlock $ forever $ do
  output <- liftBarIO action
  yieldBlockUpdate $ addIcon diskIcon output
  where
    action :: BarIO BlockOutput
    action = do
      (exitCode, output) <- liftIO $ readProcessStdout $ shell $ "df --human-readable --local --output=avail " <> T.unpack path
      return $ case exitCode of
        ExitSuccess -> createBlockOutput output
        (ExitFailure nr) -> mkErrorOutput $ "exit code " <> T.pack (show nr) <> ""
    createBlockOutput :: C8.ByteString -> BlockOutput
    createBlockOutput output = case map T.decodeUtf8 (C8.lines output) of
      [] -> mkErrorOutput "no output"
      [_header] -> mkErrorOutput "invalid output"
      (_header:values) -> mkBlockOutput $ normalText $ T.intercalate " " $ map T.strip values
