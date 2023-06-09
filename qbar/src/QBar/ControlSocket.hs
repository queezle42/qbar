{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

module QBar.ControlSocket (
  Command(..),
  CommandResult(..),
  Down,
  Up,
  addServerMirrorStream,
  listenUnixSocketAsync,
  sendBlockStream,
  sendBlockStreamStdio,
  sendIpc,
) where

import QBar.BlockOutput
import QBar.Core
import QBar.Host
import QBar.Prelude
import QBar.Time
import QBar.Utils

import Control.Concurrent (forkFinally)
import Control.Concurrent.Async
import Control.Exception (SomeException, IOException, handle, onException)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Text.Lazy (pack)
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.IO qualified as T
import Data.Time.Clock (getCurrentTime, addUTCTime)
import GHC.Generics
import Network.Socket
import Pipes
import Pipes.Aeson (decode, DecodingError)
import Pipes.Aeson.Unchecked (encode)
import Pipes.Concurrent as PC (Output, spawn, spawn', unbounded, newest, toOutput, fromInput, send, atomically)
import Pipes.Network.TCP (fromSocket, toSocket)
import Pipes.Parse
import Pipes.Prelude qualified as PP
import Pipes.Safe (catch)
import System.Directory (removeFile, doesFileExist)
import System.Environment (getEnv)
import System.Exit (exitSuccess)
import System.FilePath ((</>))
import System.IO

type CommandHandler = Command -> IO CommandResult


class (ToJSON (Up s), FromJSON (Up s), ToJSON (Down s), FromJSON (Down s)) => IsStream s where
  type Up s
  type Down s
  streamHandler :: s -> BarIO (Consumer (Up s) IO (), Producer (Down s) IO (), IO ())
  toStreamType :: s -> StreamType

  streamClient :: s -> MainOptions -> BarIO (Consumer (Up s) IO (), Producer (Down s) IO ())
  streamClient s options = do
    sock <- liftIO $ connectIpcSocket options
    streamClient' s options (toSocket sock) (fromSocket sock 4096)

  streamClient' :: s -> MainOptions -> Consumer ByteString IO () -> Producer ByteString IO () -> BarIO (Consumer (Up s) IO (), Producer (Down s) IO ())
  streamClient' s options@MainOptions{verbose} sink source = liftIO $ do
    runEffect (encode (StartStream $ toStreamType s) >-> sink)
    let up = forever (await >>= encode) >-> verbosePrintP >-> sink
    let down = decodeStreamSafe options (source >-> verbosePrintP)
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

data ReconnectMode a = ReconnectNoResend | ReconnectSendLatest a

reconnectClient :: forall up down. ReconnectMode up -> BarIO (Consumer up IO (), Producer down IO ()) -> BarIO (Consumer up IO (), Producer down IO ())
reconnectClient reconnectMode connectClient = do
  (upConsumer, upProducer) <- case reconnectMode of
    ReconnectNoResend  -> liftIO mkBroadcastP
    ReconnectSendLatest initial -> liftIO $ mkBroadcastCacheP initial

  (downOutput, downInput) <- liftIO $ spawn unbounded
  let downConsumer = toOutput downOutput
  let downProducer = fromInput downInput

  task <- barAsync $ forever $ do
    (upStreamConsumer, downStreamProducer) <- connectRetry

    liftIO $ do
      readTask <- async $ runEffect $ downStreamProducer >-> downConsumer
      writeTask <- async $ runEffect $ upProducer >-> upStreamConsumer
      void $ waitEitherCancel readTask writeTask

  liftIO $ link task

  return (upConsumer, downProducer)
  where
    connectRetry :: BarIO (Consumer up IO (), Producer down IO ())
    connectRetry = catch connectClient (\(_ :: IOException) -> liftIO (hPutStrLn stderr "Socket connection failed. Retrying...") >> reconnectDelay >> silentConnectRetry)
    silentConnectRetry :: BarIO (Consumer up IO (), Producer down IO ())
    silentConnectRetry = catch connectClient (\(_ :: IOException) -> reconnectDelay >> silentConnectRetry)
    reconnectDelay :: BarIO ()
    reconnectDelay = do
      time <- liftIO getCurrentTime
      let nextSecond = addUTCTime 1 time
      sleepUntil nextSecond


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


data StreamType
  = BlockStreamType BlockStream
  | MirrorStreamType MirrorStream
  deriving Generic

mapStreamType :: StreamType -> (forall a. IsStream a => a -> b) -> b
mapStreamType (BlockStreamType a) f = f a
mapStreamType (MirrorStreamType a) f = f a


data BlockStream = BlockStream
  deriving Generic

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
  deriving Generic

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
  deriving Generic

data Command = SetTheme T.Text | CheckServer
  deriving (Show, Generic)

data CommandResult = Success | Error Text
  deriving (Show, Generic)


ipcSocketAddress :: MainOptions -> IO FilePath
ipcSocketAddress MainOptions{socketLocation} = maybe defaultSocketPath (return . T.unpack) socketLocation
  where
    defaultSocketPath :: IO FilePath
    defaultSocketPath = do
      waylandSocketPath' <- waylandSocketPath
      maybe fallbackSocketPath return waylandSocketPath'
      where
        waylandSocketPath :: IO (Maybe FilePath)
        waylandSocketPath = handleEnvError $ do
          xdgRuntimeDir <- getEnv "XDG_RUNTIME_DIR"
          waylandDisplay <- getEnv "WAYLAND_DISPLAY"
          return $ xdgRuntimeDir </> waylandDisplay <> "-qbar"
        fallbackSocketPath :: IO FilePath
        fallbackSocketPath = do
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
sendBlockStream loadBlocks options = runBarHost blockStreamClient loadBlocks
  where
    blockStreamClient :: BarIO (Consumer [BlockOutput] IO (), Producer BlockEvent IO ())
    blockStreamClient = reconnectClient (ReconnectSendLatest []) $ streamClient BlockStream options

sendBlockStreamStdio :: BarIO () -> MainOptions -> IO ()
sendBlockStreamStdio loadBlocks options = runBarHost blockStreamClient loadBlocks
  where
    blockStreamClient :: BarIO (Consumer [BlockOutput] IO (), Producer BlockEvent IO ())
    blockStreamClient = streamClient' BlockStream options sink source
    sink :: Consumer ByteString IO ()
    sink = forever $ do
      value <- await
      -- Close when connection to upstream qbar is lost
      liftIO $ (BS.hPut stdout value >> hFlush stdout) `onException` (hPutStrLn stderr "Stdout closed" >> exitSuccess)
    source :: Producer ByteString IO ()
    source = forever $ do
      value <- liftIO (BS.hGetSome stdin 4096)
      -- Close when connection to upstream qbar is lost
      when (BS.null value) $ liftIO $ do
        hPutStrLn stderr "Stdin closed"
        exitSuccess
      yield value

addServerMirrorStream :: MainOptions -> BarIO ()
addServerMirrorStream options = do
  (blockEventConsumer, blockOutputProducer) <- reconnectClient ReconnectNoResend $ streamClient MirrorStream options

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

instance FromJSON BlockStream
instance ToJSON BlockStream

instance FromJSON Command
instance ToJSON Command

instance FromJSON CommandResult
instance ToJSON CommandResult

instance FromJSON MirrorStream
instance ToJSON MirrorStream

instance FromJSON Request
instance ToJSON Request

instance FromJSON StreamType
instance ToJSON StreamType
