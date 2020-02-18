{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module QBar.ControlSocket where

import QBar.BlockOutput
import QBar.Cli (MainOptions(..))
import QBar.Core
import QBar.Host
import QBar.Util

import Control.Exception (SomeException, handle)
import Control.Monad (forever, void, when)
import Control.Concurrent (forkFinally)
import Control.Concurrent.Async
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import System.FilePath ((</>))
import System.IO
import Data.Either (either)
import Data.Maybe (maybe)
import Data.Text.Lazy (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Network.Socket
import Pipes
import Pipes.Concurrent as PC (Output, spawn', unbounded, fromInput, send, atomically)
import Pipes.Parse
import qualified Pipes.Prelude as PP
import Pipes.Aeson (decode, DecodingError)
import Pipes.Aeson.Unchecked (encode)
import Pipes.Network.TCP (fromSocket, toSocket)
import System.Directory (removeFile, doesFileExist)
import System.Environment (getEnv)

type CommandHandler = Command -> IO CommandResult


class (ToJSON (Up s), FromJSON (Up s), ToJSON (Down s), FromJSON (Down s)) => IsStream s where
  type Up s
  type Down s
  streamHandler :: s -> BarIO (Consumer (Up s) IO (), Producer (Down s) IO (), IO ())
  toStreamType :: s -> StreamType

  streamClient :: (MonadIO m) => s -> MainOptions -> m (Consumer (Up s) IO (), Producer (Down s) IO ())
  streamClient s options@MainOptions{verbose} = do
    sock <- liftIO $ connectIpcSocket options
    runEffect (encode (StartStream $ toStreamType s) >-> toSocket sock)
    let up = forever (await >>= encode) >-> verbosePrintP >-> toSocket sock
    let down = decodeStreamSafe options (fromSocket sock 4096)
    return (up, down)
    where
      verbosePrintP :: Pipe ByteString ByteString IO ()
      verbosePrintP = if verbose then (PP.chain $ BSC.hPutStrLn stderr) else cat
  handleByteStream :: s -> MainOptions -> Producer ByteString IO () -> Consumer ByteString IO () -> BarIO ()
  handleByteStream s options up down = do
    (handleUp, handleDown, cleanup) <- streamHandler s
    readTask <- liftIO $ async $ runEffect $
      decodeStreamSafe options up >-> handleUp
    writeTask <- liftIO $ async $ runEffect $
      handleDown >-> forever (await >>= encode) >-> down
    liftIO $ do
      void $ waitEitherCancel readTask writeTask
      cleanup


decodeStreamSafe :: FromJSON v => MainOptions -> Producer ByteString IO () -> Producer v IO ()
decodeStreamSafe MainOptions{verbose} inputStream = decodeStream inputStream >-> failOnEmptyStream >-> failOnDecodingError
  where
    decodeStream :: FromJSON v => Producer ByteString IO () -> Producer (Maybe (Either DecodingError v)) IO ()
    decodeStream inputStream' = do
      (maybeDecodeResult, leftovers) <- liftIO $ runStateT decode inputStream'
      yield maybeDecodeResult
      decodeStream leftovers

    failOnEmptyStream :: Pipe (Maybe a) a IO ()
    failOnEmptyStream = failOnEmptyStream'
      where
        failOnEmptyStream' = do
          m <- await
          case m of
            Nothing -> liftIO $ when verbose $ hPutStrLn stderr "Ipc connection stream failed: Stream ended"
            Just v -> yield v >> failOnEmptyStream'

    failOnDecodingError :: Pipe (Either DecodingError a) a IO ()
    failOnDecodingError = failOnDecodingError'
      where
        failOnDecodingError' = do
          m <- await
          case m of
            Left err -> liftIO $ when verbose $ hPutStrLn stderr $ "Ipc connection stream decoding failed: " <> show err
            Right v -> yield v >> failOnDecodingError'


data StreamType = BlockStreamType BlockStream
mapStreamType :: StreamType -> (forall a. IsStream a => a -> b) -> b
mapStreamType (BlockStreamType a) f = f a


data BlockStream = BlockStream
instance IsStream BlockStream where
  type Up BlockStream = [BlockOutput]
  type Down BlockStream = BlockEvent
  toStreamType = BlockStreamType
  streamHandler _ = do
    (cache, updateC, seal) <- newCache'
    (eventOutput, eventInput, eventSeal) <- liftIO $ spawn' unbounded
    bar <- askBar
    addBlock cache
    prefix <- liftIO $ (<> "_") <$> randomIdentifier
    return (updateBarP bar >-> attachHandlerP eventOutput prefix >-> updateC, fromInput eventInput, seal >> atomically eventSeal)
    where
      attachHandlerP :: Output BlockEvent -> Text -> Pipe [BlockOutput] [BlockState] IO ()
      attachHandlerP eventOutput prefix = attachHandlerP'
        where
          attachHandlerP' :: Pipe [BlockOutput] [BlockState] IO ()
          attachHandlerP' = do
            outputs <- await
            yield $ map (\o -> maybe (noHandler o) (attachHandler o) (_blockName o)) outputs
            attachHandlerP'
          noHandler :: BlockOutput -> BlockState
          noHandler output = Just (output, Nothing)
          attachHandler :: BlockOutput -> Text -> BlockState
          attachHandler output blockName' = Just (output {_blockName = Just prefixedName}, Just patchedEvent)
            where
              patchedEvent :: BlockEventHandler
              patchedEvent event = liftIO . atomically . void $ PC.send eventOutput $ event {name = blockName'}
              prefixedName :: Text
              prefixedName = prefix <> blockName'

      updateBarP :: Bar -> Pipe a a IO ()
      updateBarP bar = do
        v <- await
        yield v
        liftIO $ updateBar' bar
        updateBarP bar


data Request = Command Command | StartStream StreamType

data Command = SetTheme TL.Text
  deriving Show

data CommandResult = Success | Error Text
  deriving Show


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

connectIpcSocket :: MainOptions -> IO Socket
connectIpcSocket options = do
  socketPath <- ipcSocketAddress options
  sock <- socket AF_UNIX Stream defaultProtocol
  connect sock $ SockAddrUnix socketPath
  return sock

$(deriveJSON defaultOptions ''Request)
$(deriveJSON defaultOptions ''Command)
$(deriveJSON defaultOptions ''CommandResult)
$(deriveJSON defaultOptions ''StreamType)
$(deriveJSON defaultOptions ''BlockStream)

sendIpc :: MainOptions -> Command -> IO ()
sendIpc options@MainOptions{verbose} command = do
  let request = Command command
  sock <- connectIpcSocket options
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

sendBlockStream :: MainOptions -> BarIO () -> IO ()
sendBlockStream = runBarHost . streamClient BlockStream


listenUnixSocketAsync :: MainOptions -> Bar -> CommandHandler -> IO (Async ())
listenUnixSocketAsync options bar commandHandler = async $ listenUnixSocket options bar commandHandler

listenUnixSocket :: MainOptions -> Bar -> CommandHandler -> IO ()
listenUnixSocket options@MainOptions{verbose} bar commandHandler = do
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
    void $ forkFinally (socketHandler conn) (handleSocketResult conn)
  where
    handleSocketResult :: Socket -> Either SomeException () -> IO ()
    handleSocketResult conn (Left err) = do
      when verbose $ hPutStrLn stderr $ "Ipc connection closed with error " <> show err
      close conn
    handleSocketResult conn (Right ()) = do
      when verbose $ hPutStrLn stderr "Ipc connection closed"
      close conn
    socketHandler :: Socket -> IO ()
    socketHandler conn = do
      when verbose $ hPutStrLn stderr "Ipc connection created"
      socketHandler' (fromSocket conn 4096) (toSocket conn)
    socketHandler' :: Producer ByteString IO () -> Consumer ByteString IO () -> IO ()
    socketHandler' producer responseConsumer = do
      (maybeDecodeResult, leftovers) <- runStateT decode producer
      -- Handle empty result
      case maybeDecodeResult of
        Nothing -> reply $ errorResponse "Empty stream"
        Just decodeResult -> case decodeResult of
          Left err -> reply $ handleError err
          Right request -> runBarIO bar $ handleRequest leftovers responseConsumer request
      where
        reply :: Producer ByteString IO () -> IO ()
        reply response = runEffect (response >-> responseConsumer)

    handleRequest :: Producer ByteString IO () -> Consumer ByteString IO () -> Request -> BarIO ()
    handleRequest _leftovers responseConsumer (Command command) = liftIO $ runEffect (handleCommand command >-> responseConsumer)
    --handleRequest leftovers responseConsumer StartBlockStream = blockStreamHandler options leftovers responseConsumer
    handleRequest leftovers responseConsumer (StartStream streamType) = mapStreamType streamType $ \s -> handleByteStream s options leftovers responseConsumer

    handleCommand :: Command -> Producer ByteString IO ()
    handleCommand command = do
      result <- liftIO $ commandHandler command
      encode result
    handleError :: DecodingError -> Producer ByteString IO ()
    handleError = encode . Error . pack . show
    errorResponse :: Text -> Producer ByteString IO ()
    errorResponse message = encode $ Error message
