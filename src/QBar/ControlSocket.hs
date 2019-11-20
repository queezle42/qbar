{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module QBar.ControlSocket where

import QBar.Cli (MainOptions(..))
-- TODO: remove dependency?
import QBar.Filter

import Control.Monad (forever, void, when)
import Control.Monad.STM (atomically)
import Control.Concurrent (forkFinally)
import Control.Concurrent.Async
import Control.Concurrent.STM.TChan (TChan, writeTChan)
import Data.Aeson.TH
import Data.Either (either)
import Data.Text.Lazy (Text, pack)
import qualified Data.Text as T
import Network.Socket
import Pipes
import Pipes.Parse
import Pipes.Aeson (decode, DecodingError)
import Pipes.Aeson.Unchecked (encode)
import Pipes.Network.TCP (fromSocket, toSocket)
import System.Directory (removeFile, doesFileExist)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO

type CommandChan = TChan Command

data Command = SetFilter Filter
  deriving Show

data SocketResponse = Success | Error Text
  deriving Show

$(deriveJSON defaultOptions ''Command)
$(deriveJSON defaultOptions ''SocketResponse)

ipcSocketAddress :: MainOptions -> IO FilePath
ipcSocketAddress MainOptions{socketLocation} = maybe defaultSocketPath (return . T.unpack) socketLocation
  where
    defaultSocketPath :: IO FilePath
    defaultSocketPath = do
      xdgRuntimeDir <- getEnv "XDG_RUNTIME_DIR"
      waylandDisplay <- getEnv "WAYLAND_DISPLAY"
      return $ xdgRuntimeDir </> waylandDisplay <> "-qbar"

sendIpc :: MainOptions -> Command -> IO ()
sendIpc options@MainOptions{verbose} request = do
  socketPath <- ipcSocketAddress options
  sock <- socket AF_UNIX Stream defaultProtocol
  connect sock $ SockAddrUnix socketPath
  runEffect $ encode request >-> toSocket sock

  decodeResult <- evalStateT decode $ fromSocket sock 4096
  maybe exitEmptyStream (either exitInvalidResult showResponse) decodeResult
  where
    exitEmptyStream :: IO ()
    exitEmptyStream = hPutStrLn stderr "Empty stream"
    exitInvalidResult :: DecodingError -> IO ()
    exitInvalidResult = hPrint stderr
    showResponse :: SocketResponse -> IO ()
    showResponse Success = when verbose $ hPutStrLn stderr "Success"
    showResponse (Error message) = hPrint stderr message

listenUnixSocketAsync :: MainOptions -> CommandChan -> IO (Async ())
listenUnixSocketAsync options commandChan = async $ listenUnixSocket options commandChan

listenUnixSocket :: MainOptions -> CommandChan -> IO ()
listenUnixSocket options commandChan = do
  socketPath <- ipcSocketAddress options
  hPutStrLn stderr $ "Creating control socket at " <> socketPath
  socketExists <- doesFileExist socketPath
  when socketExists $ removeFile socketPath
  sock <- socket AF_UNIX Stream defaultProtocol
  setCloseOnExecIfNeeded $ fdSocket sock
  bind sock (SockAddrUnix socketPath)
  listen sock 5
  forever $ do
    (conn, _) <- accept sock
    void $ forkFinally (socketHandler conn) (\_ -> close conn)
  where
    socketHandler :: Socket -> IO ()
    socketHandler sock = do
      decodeResult <- evalStateT decode $ fromSocket sock 4096
      response <- maybe (errorResponse "Empty stream") (either (errorResponse . pack . show) commandHandler) decodeResult
      let consumer = toSocket sock
      runEffect (encode response >-> consumer)
    commandHandler :: Command -> IO SocketResponse
    commandHandler command = do
      atomically $ writeTChan commandChan command
      return Success
    errorResponse :: Text -> IO SocketResponse
    errorResponse message = return $ Error message