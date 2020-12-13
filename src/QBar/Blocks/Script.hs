module QBar.Blocks.Script where

import QBar.BlockHelper
import QBar.BlockOutput
import QBar.Core
import QBar.TagParser
import QBar.Time

import Control.Exception (IOException, handle)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy.IO as TIO
import Pipes
import Pipes.Safe (catchP)
import System.Exit
import System.IO hiding (stdin, stdout)
import System.IO.Error (isEOFError)
import System.Process.Typed (Process, shell, setStdin, setStdout,
  getStdin, getStdout, closed, createPipe, readProcessStdout, startProcess, stopProcess, getExitCode)


pollScriptBlock :: Interval -> FilePath -> Block
pollScriptBlock interval path = runPollBlock' interval $ forever $ yieldBlockUpdate =<< (lift blockScriptAction)
  where
    blockScriptAction :: BarIO BlockOutput
    blockScriptAction = do
      -- The exit code is used for i3blocks signaling but ignored here (=not implemented)
      -- I am trying to replace i3blocks scripts with native haskell blocks, so I do not need it
      (exitCode, output) <- liftIO $ readProcessStdout $ shell path
      return $ case exitCode of
        ExitSuccess -> createScriptBlockOutput output
        (ExitFailure nr) -> mkErrorOutput $ "exit code " <> T.pack (show nr) <> ""
    createScriptBlockOutput :: C8.ByteString -> BlockOutput
    createScriptBlockOutput output = case map E.decodeUtf8 (C8.lines output) of
      (text:short:_) -> parseTags'' text short
      (text:_) -> parseTags' text
      [] -> emptyBlock

scriptBlock :: Bool -> FilePath -> Block
-- The outer catchP only catches errors that occur during process creation
scriptBlock clickEvents path = startScriptProcess
  where
    handleError :: Maybe ExitCode -> IOException -> Block
    handleError exitCode exc = case result of
      Left msg -> do
        signal <- liftIO newEmptyMVar
        pushBlockUpdate' (const $ liftIO $ putMVar signal ()) $
              mkErrorOutput msg
        liftIO $ takeMVar signal
        startScriptProcess
      Right x  -> x
      where
        result = case (isEOFError exc, exitCode) of
          (True, Just ExitSuccess)      -> Right exitBlock
          (True, Just (ExitFailure nr)) ->
            Left $ "exit code " <> T.pack (show nr)
          (True, Nothing) ->
            -- This will happen if we hit the race condition (see below)
            -- or the process closes its stdout without exiting.
            Left $ "exit code unavailable"
          _ -> Left $ T.pack (show exc)
    ignoreIOException :: a -> IO a -> IO a
    ignoreIOException errValue = handle $ \(_ :: IOException) -> return errValue
    handleErrorWithProcess :: (Process i o e) -> IOException -> Block
    handleErrorWithProcess process exc = do
      -- We want to know whether the process has already exited or we are
      -- killing it because of some other error. stopProcess determines
      -- that but it doesn't tell us. getExitCode is unreliable before
      -- stopProcess because it will return Nothing while the waiter threat
      -- hasn't noticed that the process is dead.
      -- Furthermore, stopProcess may fail in waitForProcess if the process
      -- has died really quickly.
      -- I don't think there is anything we can do about this. We do try
      -- to make the races less likely by waiting a bit.
      exitCode <- liftIO $ do
        threadDelay 100000
        ignoreIOException Nothing (getExitCode process)
          <* ignoreIOException () (stopProcess process)
      handleError exitCode exc
    startScriptProcess :: Block
    startScriptProcess = flip catchP (handleError Nothing) $
      if clickEvents
        then startScriptProcessWithEvents
        else startScriptProcessNoEvents
    startScriptProcessNoEvents :: Block
    startScriptProcessNoEvents = do
      let processConfig = setStdin closed $ setStdout createPipe $ shell path
      process <- startProcess processConfig
      -- The inner catchP catches errors that happen after the process has been created
      -- This handler will also make sure the process is stopped
      catchP (blockFromHandle Nothing $ getStdout process) (handleErrorWithProcess process)
    startScriptProcessWithEvents :: Block
    startScriptProcessWithEvents = do
      let processConfig = setStdin createPipe $ setStdout createPipe $ shell path
      process <- startProcess processConfig
      -- The inner catchP catches errors that happen after the process has been created
      -- This handler will also make sure the process is stopped
      blockFromHandle (Just $ getStdin process) (getStdout process)
          `catchP` handleErrorWithProcess process
    blockFromHandle :: Maybe Handle -> Handle -> Block
    blockFromHandle stdin stdout = forever $ do
      line <- liftIO $ TIO.hGetLine stdout
      let blockOutput = parseTags' line
      case stdin of
        Nothing -> pushBlockUpdate blockOutput
        Just h  -> pushBlockUpdate' (handleClick h) blockOutput
    handleClick :: Handle -> BlockEventHandler
    handleClick stdin ev = liftIO $ do
      C8.hPutStrLn stdin $ encode ev
      hFlush stdin
