{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module QBar.ControlSocket where

import QBar.Cli (MainOptions(..))
import QBar.Core
import QBar.BlockOutput

import Control.Exception (handle)
import Control.Monad (forever, void, when)
import Control.Concurrent (forkFinally)
import Control.Concurrent.Async
import Data.Aeson.TH
import Data.ByteString (ByteString)
import System.FilePath ((</>))
import System.IO
import Data.Either (either)
import Data.Maybe (maybe)
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

type CommandHandler = Command -> IO CommandResult

data Request = Command Command | ConnectBarHost
  deriving Show

data Command = SetTheme TL.Text
  deriving Show

data CommandResult = Success | Error Text
  deriving Show

$(deriveJSON defaultOptions ''Request)
$(deriveJSON defaultOptions ''Command)
$(deriveJSON defaultOptions ''CommandResult)


ipcSocketAddress :: MainOptions -> IO FilePath
ipcSocketAddress MainOptions{socketLocation} = maybe defaultSocketPath (return . T.unpack) socketLocation
  where
    defaultSocketPath :: IO FilePath
    defaultSocketPath = do
      waylandSocketPath' <- waylandSocketPath
      maybe (maybe headlessSocketPath return =<< i3SocketPath) return waylandSocketPath'
      where
        waylandSocketPath :: IO (Maybe FilePath)
        waylandSocketPath = handleEnvError $ do
          xdgRuntimeDir <- getEnv "XDG_RUNTIME_DIR"
          waylandDisplay <- getEnv "WAYLAND_DISPLAY"
          return $ xdgRuntimeDir </> waylandDisplay <> "-qbar"
        i3SocketPath :: IO (Maybe FilePath)
        i3SocketPath = handleEnvError $ do
          i3SocketPath' <- getEnv "I3_SOCKET_PATH"
          return $ i3SocketPath' <> "-qbar"
        headlessSocketPath :: IO FilePath
        headlessSocketPath = do
          xdgRuntimeDir <- getEnv "XDG_RUNTIME_DIR"
          return $ xdgRuntimeDir </> "qbar"
    handleEnvError :: IO FilePath -> IO (Maybe FilePath)
    handleEnvError = handle (const $ return Nothing :: IOError -> IO (Maybe FilePath)) . fmap Just

sendIpc :: MainOptions -> Command -> IO ()
sendIpc options@MainOptions{verbose} command = do
  let request = Command command
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
    showResponse :: CommandResult -> IO ()
    showResponse Success = when verbose $ hPutStrLn stderr "Success"
    showResponse (Error message) = hPrint stderr message

listenUnixSocketAsync :: MainOptions -> CommandHandler -> IO (Async ())
listenUnixSocketAsync options commandHandler = async $ listenUnixSocket options commandHandler

listenUnixSocket :: MainOptions -> CommandHandler -> IO ()
listenUnixSocket options commandHandler = do
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
    streamHandler producer responseConsumer = do
      (maybeDecodeResult, leftovers) <- runStateT decode producer
      -- Handle empty result
      case maybeDecodeResult of
        Nothing -> reply $ errorResponse "Empty stream"
        Just decodeResult -> case decodeResult of
          Left err -> reply $ handleError err
          Right request -> handleRequest leftovers responseConsumer request
      where
        reply :: Producer ByteString IO () -> IO ()
        reply response = runEffect (response >-> responseConsumer)

    handleRequest :: Producer ByteString IO () -> Consumer ByteString IO () -> Request -> IO ()
    handleRequest _leftovers responseConsumer (Command command) = runEffect (handleCommand command >-> responseConsumer)
    --handleRequest leftovers Block = addBlock $ handleBlockStream leftovers
    handleRequest _leftovers _responseConsumer ConnectBarHost = error "TODO"

    handleCommand :: Command -> Producer ByteString IO ()
    handleCommand command = do
      result <- liftIO $ commandHandler command
      encode result
    handleError :: DecodingError -> Producer ByteString IO ()
    handleError = encode . Error . pack . show
    errorResponse :: Text -> Producer ByteString IO ()
    errorResponse message = encode $ Error message

handleBlockStream :: Producer ByteString IO () -> PushBlock
handleBlockStream producer = do
  (decodeResult, leftovers) <- liftIO $ runStateT decode producer
  maybe exitBlock (either (const exitBlock) (handleParsedBlock leftovers)) decodeResult
  where
    handleParsedBlock :: Producer ByteString IO () -> String -> PushBlock
    handleParsedBlock leftovers update = do
      updateBlock $ mkBlockOutput . normalText $ TL.pack update
      handleBlockStream leftovers
