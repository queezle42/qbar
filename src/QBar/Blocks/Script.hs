module QBar.Blocks.Script where

import QBar.BlockOutput
import QBar.Core
import QBar.TagParser

import Control.Exception (IOException)
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy.IO as TIO
import Pipes
import Pipes.Safe (catchP)
import System.Exit
import System.IO
import System.Process.Typed (Process, shell, setStdin, setStdout,
  getStdout, closed, createPipe, readProcessStdout, startProcess, stopProcess)


scriptBlock :: FilePath -> PullBlock
scriptBlock path = forever $ updateBlock =<< (lift blockScriptAction)
  where
    blockScriptAction :: BarIO BlockOutput
    blockScriptAction = do
      -- The exit code is used for i3blocks signaling but ignored here (=not implemented)
      -- I am trying to replace i3blocks scripts with native haskell blocks, so I do not need it
      (exitCode, output) <- liftIO $ readProcessStdout $ shell path
      return $ case exitCode of
        ExitSuccess -> createScriptBlockOutput output
        (ExitFailure nr) -> case nr of
          _ -> mkErrorOutput $ "exit code " <> T.pack (show nr) <> ""
    createScriptBlockOutput :: C8.ByteString -> BlockOutput
    createScriptBlockOutput output = case map E.decodeUtf8 (C8.lines output) of
      (text:short:_) -> parseTags'' text short
      (text:_) -> parseTags' text
      [] -> emptyBlock

persistentScriptBlock :: FilePath -> PushBlock
-- The outer catchP only catches errors that occur during process creation
persistentScriptBlock path = catchP startScriptProcess handleError
  where
    handleError :: IOException -> PushBlock
    handleError e = do
      updateBlock . mkErrorOutput $ T.pack (show e)
      exitBlock
    handleErrorWithProcess :: (Process i o e) -> IOException -> PushBlock
    handleErrorWithProcess process e = do
      stopProcess process
      handleError e
    startScriptProcess :: PushBlock
    startScriptProcess = do
      let processConfig = setStdin closed $ setStdout createPipe $ shell path
      process <- startProcess processConfig
      -- The inner catchP catches errors that happen after the process has been created
      -- This handler will also make sure the process is stopped
      catchP (blockFromHandle $ getStdout process) (handleErrorWithProcess process)
    blockFromHandle :: Handle -> PushBlock
    blockFromHandle handle = forever $ do
      line <- liftIO $ TIO.hGetLine handle
      updateBlock $ parseTags' line
