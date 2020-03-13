{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

module QBar.ControlSocket where

import QBar.BlockOutput
import QBar.Core
import QBar.Host
import QBar.Util

import Control.Concurrent (forkFinally)
import Control.Concurrent.Async
import Control.Exception (SomeException, handle, catch)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH
import qualified Data.ByteString.Char8 as BSC
import System.FilePath ((</>))
import System.IO
import Data.Text.Lazy (pack)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Network.Socket
import Pipes
import Pipes.Concurrent as PC (Output, spawn, spawn', unbounded, newest, toOutput, fromInput, send, atomically)
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

  streamClient :: s -> MainOptions -> BarIO (Consumer (Up s) IO (), Producer (Down s) IO ())
  streamClient s options@MainOptions{verbose} = do
    sock <- liftIO $ connectIpcSocket options
    runEffect (encode (StartStream $ toStreamType s) >-> toSocket sock)
    let up = forever (await >>= encode) >-> verbosePrintP >-> toSocket sock
    let down = decodeStreamSafe options (fromSocket sock 4096 >-> verbosePrintP)
    return (up, down)
    where
      verbosePrintP :: Pipe ByteString ByteString IO ()
      verbosePrintP = if verbose then PP.chain $ BSC.hPutStrLn stderr else cat
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


data StreamType = BlockStreamType BlockStream | MirrorStreamType MirrorStream
mapStreamType :: StreamType -> (forall a. IsStream a => a -> b) -> b
mapStreamType (BlockStreamType a) f = f a
mapStreamType (MirrorStreamType a) f = f a


data BlockStream = BlockStream
instance IsStream BlockStream where
  type Up BlockStream = [BlockOutput]
  type Down BlockStream = BlockEvent
  toStreamType = BlockStreamType
  streamHandler :: BlockStream -> BarIO (Consumer [BlockOutput] IO (), Producer BlockEvent IO (), IO ())
  streamHandler _ = do
    (cache, updateCacheC, sealCache) <- newCache'
    (eventOutput, eventInput, eventSeal) <- liftIO $ spawn' unbounded
    bar <- askBar
    addBlockCache cache
    prefix <- liftIO $ (<> "_") <$> randomIdentifier
    let blockConsumer = updateBarP bar >-> attachHandlerP eventOutput prefix >-> updateCacheC
    let eventProducer = fromInput eventInput
    let seal = sealCache >> atomically eventSeal >> updateBarDefault' bar
    return (blockConsumer, eventProducer, seal)
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
      updateBarP bar = forever $ await >>= yield >> liftIO (updateBarDefault' bar)


data MirrorStream = MirrorStream
instance IsStream MirrorStream where
  type Up MirrorStream = BlockEvent
  type Down MirrorStream = [BlockOutput]
  toStreamType = MirrorStreamType
  streamHandler :: MirrorStream -> BarIO (Consumer BlockEvent IO (), Producer [BlockOutput] IO (), IO ())
  streamHandler _ = do
    (eventOutput, eventInput, eventSeal) <- liftIO $ spawn' unbounded
    (blockOutput, blockInput, blockSeal) <- liftIO $ spawn' $ newest 1
    let seal = atomically $ eventSeal >> blockSeal

    attachBarOutput (toOutput blockOutput, fromInput eventInput)
    return (toOutput eventOutput, fromInput blockInput, seal)


data Request = Command Command | StartStream StreamType

data Command = SetTheme T.Text | CheckServer
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
$(deriveJSON defaultOptions ''MirrorStream)

sendIpc :: Command -> MainOptions -> IO ()
sendIpc command options@MainOptions{verbose} = do
  result <- sendIpc' command options
  case result of
    Left err -> T.hPutStrLn stderr err
    Right () -> when verbose $ hPutStrLn stderr "Success"

sendIpc' :: Command -> MainOptions -> IO (Either Text ())
sendIpc' command options = catch sendCommand handleException
  where
    sendCommand :: IO (Either Text ())
    sendCommand = do
      sock <- connectIpcSocket options
      runEffect $ encode (Command command) >-> toSocket sock

      decodeResult <- evalStateT decode $ fromSocket sock 4096
      return $ maybe onEmptyStream (either onInvalidResult showResponse) decodeResult

    handleException :: SomeException -> IO (Either Text ())
    handleException = return . Left . T.pack . show
    onEmptyStream :: Either Text ()
    onEmptyStream = Left "Empty stream"
    onInvalidResult :: DecodingError -> Either Text ()
    onInvalidResult = Left . T.pack . show
    showResponse :: CommandResult -> Either Text ()
    showResponse Success = Right ()
    showResponse (Error message) = Left message


sendBlockStream :: BarIO () -> MainOptions -> IO ()
sendBlockStream loadBlocks options = runBarHost (streamClient BlockStream options) loadBlocks

addServerMirrorStream :: MainOptions -> BarIO ()
addServerMirrorStream options = do
  (blockEventConsumer, blockOutputProducer) <- streamClient MirrorStream options

  (eventOutput, eventInput) <- liftIO $ spawn unbounded
  bar <- askBar

  task <- liftIO $ async $ runEffect $ fromInput eventInput >-> blockEventConsumer
  liftIO $ link task
  prefix <- liftIO $ (<> "_") <$> randomIdentifier
  addBlockCache $ newCacheIO (blockOutputProducer >-> updateBarP bar >-> attachHandlerP eventOutput prefix)
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
    updateBarP bar = forever $ await >>= yield >> liftIO (updateBarDefault' bar)



listenUnixSocketAsync :: MainOptions -> Bar -> CommandHandler -> IO (Async ())
listenUnixSocketAsync options bar commandHandler = async $ listenUnixSocket options bar commandHandler

listenUnixSocket :: MainOptions -> Bar -> CommandHandler -> IO ()
listenUnixSocket options@MainOptions{verbose} bar commandHandler = do
  socketPath <- ipcSocketAddress options
  socketExists <- doesFileExist socketPath
  if socketExists
    then do
      socketTestResult <- sendIpc' CheckServer options
      case socketTestResult of
        Right _ -> hPutStrLn stderr $ "Could not create control socket at " <> socketPath <> ": another server is already running"
        Left _ -> do
          removeFile socketPath
          listenUnixSocket' socketPath
    else
      listenUnixSocket' socketPath
  where
    listenUnixSocket' :: FilePath -> IO b
    listenUnixSocket' socketPath = do
      hPutStrLn stderr $ "Creating control socket at " <> socketPath
      sock <- socket AF_UNIX Stream defaultProtocol
#if MIN_VERSION_network(3,0,0)
      withFdSocket sock setCloseOnExecIfNeeded
#else
      setCloseOnExecIfNeeded $ fdSocket sock
#endif
      bind sock (SockAddrUnix socketPath)
      listen sock 5
      forever $ do
        (conn, _) <- accept sock
        void $ forkFinally (socketHandler conn) (handleSocketResult conn)

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
    handleRequest leftovers responseConsumer (StartStream streamType) = mapStreamType streamType $ \s -> handleByteStream s options leftovers responseConsumer

    handleCommand :: Command -> Producer ByteString IO ()
    handleCommand command = do
      result <- liftIO $ commandHandler command
      encode result
    handleError :: DecodingError -> Producer ByteString IO ()
    handleError = encode . Error . pack . show
    errorResponse :: Text -> Producer ByteString IO ()
    errorResponse message = encode $ Error message
