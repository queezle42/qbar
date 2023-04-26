module QBar.Qubes.AdminAPI where

import QBar.Prelude

import Control.Monad (forM_, guard)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Char (isAlphaNum)
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Network.HostName
import Numeric (showHex, readHex)
import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.Safe as P
import System.IO (Handle, hSetBinaryMode)
import System.Process.Typed
import Text.Read (readMaybe)

data QubesAdminReturn
  = Ok { okContent :: BL.ByteString }
  | Event { evSubject :: BL.ByteString, evEvent :: BL.ByteString, evProperties :: [(BL.ByteString, BL.ByteString)] }
  | Exception { excType :: BL.ByteString, excTraceback :: BL.ByteString, excFormatString :: BL.ByteString, excFields :: [BL.ByteString] }
  deriving (Eq, Ord, Show, Read)

putLazyByteStringNul :: BL.ByteString -> Put
putLazyByteStringNul x = do
  when (0 `BL.elem` x) $ error "String must not contain any \\x00 bytes"
  putLazyByteString x
  putWord8 0x00

instance Binary QubesAdminReturn where
  put Ok {okContent} = do
    putWord8 0x30 >> putWord8 0x00
    putLazyByteString okContent
  put Event {evSubject, evEvent, evProperties} = do
    putWord8 0x31 >> putWord8 0x00
    putLazyByteStringNul evSubject
    putLazyByteStringNul evEvent
    forM_ evProperties $ \(k, v) -> do
      putLazyByteStringNul k
      putLazyByteStringNul v
    putWord8 0x00
  put Exception {excType, excTraceback, excFormatString, excFields} = do
    putWord8 0x32 >> putWord8 0x00
    putLazyByteStringNul excType
    putLazyByteStringNul excTraceback
    putLazyByteStringNul excFormatString
    forM_ excFields putLazyByteStringNul
    putWord8 0x00
  get = do
    msgType <- getWord8
    zero <- getWord8
    case (msgType, zero) of
      (0x30, 0x00) -> Ok <$> getRemainingLazyByteString
      (0x31, 0x00) -> Event <$> getLazyByteStringNul <*> getLazyByteStringNul <*> getPairs
      (0x32, 0x00) -> Exception <$> getLazyByteStringNul <*> getLazyByteStringNul <*> getLazyByteStringNul <*> getFields
      _ -> fail $ "unsupported message type " <> show msgType <> ", " <> show zero
    where
      getPairs = untilZeroByte $ (,) <$> getLazyByteStringNul <*> getLazyByteStringNul
      getFields = untilZeroByte getLazyByteStringNul
      untilZeroByte inner = lookAhead getWord8 >>= \case
        0x00 -> getWord8 >> return []
        _    -> inner >>= \x -> (x:) <$> untilZeroByte inner

qubesAdminConnect :: BL.ByteString -> [BL.ByteString] -> IO (Process () Handle ())
qubesAdminConnect serviceName args = do
  hostname <- getHostName
  let concatArgs sep = mconcat (map (sep<>) args)
  let cmd = if hostname == "dom0"
      then "qubesd-query dom0 " <> serviceName <> " dom0" <> concatArgs " "
      else "qrexec-client-vm dom0 " <> serviceName <> concatArgs "+"
  --NOTE qubesd-query and qrexec-client-vm don't like it if their input
  --     is closed rather than empty.
  --     hangs: qrexec-client-vm dom0 admin.vm.List <&-
  --     works: qrexec-client-vm dom0 admin.vm.List </dev/null
  let processConfig = setStdin nullStream $ setStdout createPipe $ shell $ BLC.unpack cmd
  startProcess processConfig

qubesTryAdminCall :: BL.ByteString -> [BL.ByteString] -> IO QubesAdminReturn
qubesTryAdminCall serviceName args = do
  process <- qubesAdminConnect serviceName args
  let stdout = getStdout process
  hSetBinaryMode stdout True
  reply <- decode <$> BL.hGetContents stdout
  case reply of
    Ok {} -> return reply
    Exception {} -> return reply
    Event {} -> fail "service has returned events instead of a reply"

qubesAdminCall :: BL.ByteString -> [BL.ByteString] -> IO BL.ByteString
qubesAdminCall serviceName args = qubesTryAdminCall serviceName args >>= extract where
  extract Ok {okContent} = return okContent
  extract x@Exception {} = fail $ "service has returned an exception: " <> show x
  extract Event {} = fail "service has returned events instead of a reply"

qubesAdminCallP :: forall m. (P.MonadSafe m, MonadIO m, MonadFail m)
  => BL.ByteString -> [BL.ByteString] -> Producer QubesAdminReturn m ()
qubesAdminCallP serviceName args = do
  process <- liftIO $ qubesAdminConnect serviceName args
  let stdout = getStdout process
  liftIO $ hSetBinaryMode stdout True
  let go :: Decoder QubesAdminReturn -> Producer QubesAdminReturn m ()
      go = \case
        Done remainder _ value -> do
          yield value
          go $ pushChunk (runGetIncremental get) remainder
        d@(Partial _) -> do
          chunk <- liftIO $ BS.hGetSome stdout 1024
          if not (BS.null chunk)
            then go $ pushChunk d chunk
            else case pushEndOfInput d of
              Done _ _ value -> yield value
              _              -> return ()
        Fail _ _ msg ->
          fail $ "decoding reply from QubesAdmin failed: " <> msg
  go (runGetIncremental get)
    `P.finally` stopProcess process

qubesAdminEvents :: forall m. (P.MonadSafe m, MonadIO m, MonadFail m)
  => BL.ByteString -> [BL.ByteString] -> Producer QubesAdminReturn m ()
qubesAdminEvents serviceName args = qubesAdminCallP serviceName args >-> onlyEvents
  where
    onlyEvents :: Pipe QubesAdminReturn QubesAdminReturn m ()
    onlyEvents = forever $ await >>= \reply -> case reply of
        Ok {} -> fail "service has returned OK instead of events"
        Exception {} -> fail $ "service has returned an exception: " ++ show reply
        Event {} -> yield reply

qubesVMStatsRaw :: forall m. (P.MonadSafe m, MonadIO m, MonadFail m)
  => Producer QubesAdminReturn m ()
qubesVMStatsRaw = qubesAdminEvents "admin.vm.Stats" []

data QubesVMStats = QubesVMStats { statsVMName :: BL.ByteString, memoryKB :: Int, cpuTime :: Int, cpuUsageRaw :: Int, cpuUsage :: Int }
  deriving (Eq, Ord, Show, Read)

qubesVMStats :: forall m. (P.MonadSafe m, MonadIO m, MonadFail m)
  => Producer QubesVMStats m ()
qubesVMStats = qubesVMStatsRaw >-> P.mapFoldable parse where
  parse :: QubesAdminReturn -> Maybe QubesVMStats
  parse Event {evSubject, evEvent, evProperties}
    | evEvent == "connection-established" = Nothing
    | evEvent == "vm-stats" = Just $ addProperties evProperties $ QubesVMStats evSubject absent absent absent absent
    | otherwise = Nothing  -- shouldn't happen -> report error?
  parse _ = Nothing  -- shouldn't happen -> report error?

  absent = (-1)
  readBL = read . BLC.unpack

  addProperties :: [(BL.ByteString, BL.ByteString)] -> QubesVMStats -> QubesVMStats
  addProperties (("memory_kb",     x) : xs) st = addProperties xs $ st { memoryKB    = readBL x }
  addProperties (("cpu_time",      x) : xs) st = addProperties xs $ st { cpuTime     = readBL x }
  addProperties (("cpu_usage_raw", x) : xs) st = addProperties xs $ st { cpuUsageRaw = readBL x }
  addProperties (("cpu_usage",     x) : xs) st = addProperties xs $ st { cpuUsage    = readBL x }
  addProperties (_ : xs) st = addProperties xs st
  addProperties [] st = st

data QubesEvent
  = OtherEvent QubesAdminReturn
  | DomainPreStart { domainName :: BL.ByteString, startGuid :: Maybe Bool }
  | DomainStart { domainName :: BL.ByteString, startGuid :: Maybe Bool }
  | DomainUnpaused { domainName :: BL.ByteString }
  | DomainStopped { domainName :: BL.ByteString }
  | DomainShutdown { domainName :: BL.ByteString }
  | DomainUpdatesAvailable { domainName :: BL.ByteString, updatesAvailable :: Bool, updatesAvailableOld :: Bool }
  | DomainStartFailed { domainName :: BL.ByteString, reason :: BL.ByteString }
  | PropertySet { domainName :: BL.ByteString, changedProperty :: BL.ByteString, newValue :: BL.ByteString, oldValue :: BL.ByteString }
  | PropertyDel { domainName :: BL.ByteString, changedProperty :: BL.ByteString, oldValue :: BL.ByteString } -- reset to default value
  deriving (Eq, Ord, Show, Read)

qubesEventsRaw :: forall m. (P.MonadSafe m, MonadIO m, MonadFail m)
  => Producer QubesAdminReturn m ()
qubesEventsRaw = qubesAdminEvents "admin.Events" []

qubesEvents :: forall m. (P.MonadSafe m, MonadIO m, MonadFail m)
  => Producer QubesEvent m ()
qubesEvents = qubesEventsRaw >-> P.mapFoldable parse where
  parse :: QubesAdminReturn -> Maybe QubesEvent
  parse Event {evEvent="connection-established"} = Nothing
  parse ev@(Event {evSubject, evEvent, evProperties}) =
    Just $ case evEvent of
      "domain-pre-start" -> DomainPreStart evSubject (boolProp "start_guid")
      "domain-start" -> DomainStart evSubject (boolProp "start_guid")
      "domain-unpaused" -> DomainUnpaused evSubject
      "domain-stopped" -> DomainStopped evSubject
      "domain-shutdown" -> DomainShutdown evSubject
      "domain-feature-set:updates-available" ->
        DomainUpdatesAvailable evSubject (boolPropViaInt "value") (boolPropViaInt "oldvalue")
      "domain-start-failed" ->
        DomainStartFailed evSubject (fromMaybe "" $ getProp "reason")
      _ -> case BLC.break (==':') evEvent of
        ("property-set", _) ->
          PropertySet evSubject (fromMaybe "" $ getProp "name") (fromMaybe "" $ getProp "newvalue") (fromMaybe "" $ getProp "oldvalue")
        ("property-del", _) ->
          PropertyDel evSubject (fromMaybe "" $ getProp "name") (fromMaybe "" $ getProp "oldvalue")
        _ -> OtherEvent ev
    where
      getProp :: BL.ByteString -> Maybe BL.ByteString
      getProp name = lookup name evProperties
      readProp :: Read a => BL.ByteString -> Maybe a
      readProp name = read . BLC.unpack <$> getProp name
      intProp :: BL.ByteString -> Maybe Int
      intProp = readProp
      boolProp :: BL.ByteString -> Maybe Bool
      boolProp = readProp
      boolPropViaInt :: BL.ByteString -> Bool
      boolPropViaInt = fromMaybe False . fmap (/=0) . intProp
  parse _ = Nothing  -- shouldn't happen -> report error?

printEvents  :: Show a => Producer a (P.SafeT IO) () -> IO ()
printEvents prod = P.runSafeT $ runEffect $ prod >-> (forever $ await >>= liftIO . print)

data QubesVMState = VMRunning | VMHalted | UnknownState
  deriving (Eq, Ord, Enum)
data QubesVMClass = AdminVM | AppVM | TemplateVM | DispVM | StandaloneVM | UnknownClass
  deriving (Eq, Ord, Enum, Show, Read)
data QubesVMInfo = QubesVMInfo { vmState :: QubesVMState, vmClass :: QubesVMClass }
  deriving (Eq, Ord, Show, Read)

instance Show QubesVMState where
  show VMRunning = "Running"
  show VMHalted = "Halted"
  show UnknownState = "??"

instance Read QubesVMState where
  readsPrec _ s = [(value, remainder)]
    where
      (word, remainder) = span isAlphaNum s
      value = case word of
        "Running" -> VMRunning
        "Halted" -> VMHalted
        _ -> UnknownState

qubesAdminCallLines :: BL.ByteString -> [BL.ByteString] -> IO [BL.ByteString]
qubesAdminCallLines serviceName args = qubesAdminCall serviceName args >>= parse
  where
    parse reply = BLC.split '\n' reply
      & filter (/="")
      & return

qubesListVMs :: IO (Map.Map BL.ByteString QubesVMInfo)
qubesListVMs = parse <$> qubesAdminCallLines "admin.vm.List" []
  where
    parse = Map.fromList . map parseLine
    parseLine line =
      (vmName, QubesVMInfo (readPropEmpty "state") (tryReadProp "class" & fromMaybe UnknownClass))
      where
        (vmName : propsRaw) = BLC.split ' ' line
        props = map (fmap BLC.tail . BLC.break (=='=')) propsRaw
        getProp :: BL.ByteString -> Maybe BL.ByteString
        getProp name = lookup name props
        readPropEmpty :: Read a => BL.ByteString -> a
        readPropEmpty name = read . BLC.unpack . fromMaybe "" $ getProp name
        tryReadProp :: Read a => BL.ByteString -> Maybe a
        tryReadProp name = readMaybe . BLC.unpack =<< getProp name

qubesListVMsP :: forall m. (P.MonadSafe m, MonadIO m, MonadFail m)
  => Producer (Map.Map BL.ByteString QubesVMInfo) m ()
qubesListVMsP = liftIO qubesListVMs >>= yield >> qubesEvents >-> P.mapM (const $ liftIO qubesListVMs)

data QubesPropertyInfo = QubesPropertyInfo { propIsDefault :: Bool, propType :: BL.ByteString, propValue :: BL.ByteString }
  deriving (Eq, Ord, Show, Read)

qubesGetProperty :: BL.ByteString -> IO QubesPropertyInfo
qubesGetProperty name = parse <$> qubesAdminCall "admin.property.Get" [name]
  where
    parse reply = QubesPropertyInfo (isDefault == "default=True") (BL.drop 5 typeStr) value
      where
        splitOn ch = fmap BLC.tail . BLC.break (==ch)
        (isDefault, (typeStr, value)) = splitOn ' ' reply & fmap (splitOn ' ')

qubesListPropertyNames :: IO [BL.ByteString]
qubesListPropertyNames = qubesAdminCallLines "admin.property.List" []

qubesListProperties :: IO [(BL.ByteString, QubesPropertyInfo)]
qubesListProperties = qubesListLabelNames >>= mapM (toSndM qubesGetProperty)
  where
    toSndM :: Applicative m => (a -> m b) -> a -> m (a, b)
    toSndM f x = sequenceA (x, f x)

qubesGetDefaultPool :: IO BL.ByteString
qubesGetDefaultPool = propValue <$> qubesGetProperty "default_pool"

qubesGetPoolInfo :: BL.ByteString -> IO [(BL.ByteString, BL.ByteString)]
qubesGetPoolInfo name = map parseLine <$> qubesAdminCallLines "admin.pool.Info" [name]
  where
    parseLine = fmap BLC.tail . BLC.break (=='=')

qubesUsageOfDefaultPool :: IO (Maybe Int, Maybe Int)
qubesUsageOfDefaultPool = qubesGetDefaultPool >>= qubesGetPoolInfo >>= extract
  where
    extract props = return (tryReadProp "usage" props, tryReadProp "size" props)
    tryReadProp :: Read a => BL.ByteString -> [(BL.ByteString, BL.ByteString)] -> Maybe a
    tryReadProp name props = readMaybe . BLC.unpack =<< lookup name props

newtype QubesLabelColor = QubesLabelColor { fromQubesLabelColor :: Int }
  deriving (Eq, Ord)

instance Show QubesLabelColor where
  showsPrec _ (QubesLabelColor x) = \s -> "0x" <> pad 6 (showHex x "") <> s
    where pad l s = replicate (l - length s) '0' <> s

instance Read QubesLabelColor where
  readsPrec _ ('0' : 'x' : xs) = do
    let (num, remainder) = splitAt 6 xs
    guard $ length num == 6
    (num', []) <- readHex num
    [(QubesLabelColor num', remainder)]
  readsPrec _ _ = []

qubesGetLabelColor :: BL.ByteString -> IO QubesLabelColor
qubesGetLabelColor name = read . BLC.unpack <$> qubesAdminCall "admin.label.Get" [name]

qubesListLabelNames :: IO [BL.ByteString]
qubesListLabelNames = qubesAdminCallLines  "admin.label.List" []

qubesListLabels :: IO [(BL.ByteString, QubesLabelColor)]
qubesListLabels = qubesListLabelNames >>= mapM (toSndM qubesGetLabelColor)
  where
    toSndM :: Applicative m => (a -> m b) -> a -> m (a, b)
    toSndM f x = sequenceA (x, f x)

qubesMonitorProperty :: forall m. (P.MonadSafe m, MonadIO m, MonadFail m)
  => Producer QubesEvent m () -> BL.ByteString -> Producer QubesPropertyInfo m ()
qubesMonitorProperty events name = events >-> P.filter isRelevant >-> fetchValue
  where
    fetchValue = liftIO (qubesGetProperty name) >>= go
    go x = do
      yield x
      ev <- await
      case ev of
        PropertySet {newValue} -> go $ x { propIsDefault = False, propValue = newValue }
        PropertyDel {} -> fetchValue
        _ -> go x
    isRelevant PropertySet {changedProperty} = name == changedProperty
    isRelevant PropertyDel {changedProperty} = name == changedProperty
    isRelevant _ = False
