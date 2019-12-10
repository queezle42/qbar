{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module QBar.ControlSocket where

import QBar.Cli (MainOptions(..))
import QBar.Core
-- TODO: remove dependency?
import QBar.Filter
import QBar.BlockText

import Control.Monad (forever, void, when)
import Control.Monad.STM (atomically)
import Control.Concurrent (forkFinally)
import Control.Concurrent.Async
import Control.Concurrent.STM.TChan (TChan, writeTChan)
import Data.Aeson.TH
import Data.ByteString (ByteString)
import Data.Either (either)
import Data.Text.Lazy (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
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
  | Block
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
      -- TODO: fallback to I3_SOCKET_PATH if WAYLAND_DISPLAY is not set.
      -- If both are not set it might be useful to fall back to XDG_RUNTIME_DIR/qbar, so qbar can run headless (eg. for tests)
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
    socketHandler sock = streamHandler (fromSocket sock 4096) (toSocket sock)
    streamHandler :: Producer ByteString IO () -> Consumer ByteString IO () -> IO ()
    streamHandler producer consumer = do
      (decodeResult, leftovers) <- runStateT decode producer
      response <- maybe (errorResponse "Empty stream") (either handleError (handleCommand leftovers)) decodeResult
      runEffect (encode response >-> consumer)
    handleCommand :: Producer ByteString IO () -> Command -> IO SocketResponse
    handleCommand _ Block = error "TODO" -- addBlock $ handleBlockStream leftovers
    handleCommand _ command = do
      atomically $ writeTChan commandChan command
      return Success
    handleError :: DecodingError -> IO SocketResponse
    handleError = return . Error . pack . show
    errorResponse :: Text -> IO SocketResponse
    errorResponse message = return $ Error message

handleBlockStream :: Producer ByteString IO () -> PushBlock
handleBlockStream producer = do
  (decodeResult, leftovers) <- liftIO $ runStateT decode producer
  maybe exitBlock (either (\_ -> exitBlock) (handleParsedBlock leftovers)) decodeResult
  where
    handleParsedBlock :: Producer ByteString IO () -> String -> PushBlock
    handleParsedBlock leftovers update = do
      yield $ createBlock . normalText $ TL.pack update
      handleBlockStream leftovers
