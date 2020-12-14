module QBar.Qubes.AdminAPI where

import Control.Monad (forM_)
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
  when (0 `BL.elem` x) $ error "string mustn't contain any \\x00 bytes"
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

qubesAdminConnect :: BL.ByteString -> IO (Process () Handle ())
qubesAdminConnect serviceName = do
  hostname <- getHostName
  let cmd = if hostname == "dom0"
      then "qubesd-query dom0 " <> serviceName <> " dom0"
      else "qrexec-client-vm dom0 " <> serviceName
  --NOTE qubesd-query and qrexec-client-vm don't like it if their input
  --     is closed rather than empty.
  --     hangs: qrexec-client-vm dom0 admin.vm.List <&-
  --     works: qrexec-client-vm dom0 admin.vm.List </dev/null
  let processConfig = setStdin nullStream $ setStdout createPipe $ shell $ BLC.unpack cmd
  startProcess processConfig

qubesAdminCall :: BL.ByteString -> IO QubesAdminReturn
qubesAdminCall serviceName = do
  process <- qubesAdminConnect serviceName
  let stdout = getStdout process
  hSetBinaryMode stdout True
  reply <- decode <$> BL.hGetContents stdout
  case reply of
    Ok {} -> return reply
    Exception {} -> return reply
    Event {} -> fail "service has returned events instead of a reply"

qubesAdminCallP :: BL.ByteString -> Producer QubesAdminReturn (P.SafeT IO) ()
qubesAdminCallP serviceName = do
  process <- liftIO $ qubesAdminConnect serviceName
  let stdout = getStdout process
  liftIO $ hSetBinaryMode stdout True
  let go :: Decoder QubesAdminReturn -> Producer QubesAdminReturn (P.SafeT IO) ()
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

qubesAdminEvents :: BL.ByteString -> Producer QubesAdminReturn (P.SafeT IO) ()
qubesAdminEvents serviceName = qubesAdminCallP serviceName >-> onlyEvents
  where
    onlyEvents :: Pipe QubesAdminReturn QubesAdminReturn (P.SafeT IO) ()
    onlyEvents = forever $ await >>= \reply -> case reply of
        Ok {} -> fail "service has returned OK instead of events"
        Exception {} -> fail $ "service has returned an exception: " ++ show reply
        Event {} -> yield reply

qubesVMStatsRaw :: Producer QubesAdminReturn (P.SafeT IO) ()
qubesVMStatsRaw = qubesAdminEvents "admin.vm.Stats"

data QubesVMStats = QubesVMStats { statsVMName :: BL.ByteString, memoryKB :: Int, cpuTime :: Int, cpuUsageRaw :: Int, cpuUsage :: Int }
  deriving (Eq, Ord, Show, Read)

qubesVMStats :: Producer QubesVMStats (P.SafeT IO) ()
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
  deriving (Eq, Ord, Show, Read)

qubesEventsRaw :: Producer QubesAdminReturn (P.SafeT IO) ()
qubesEventsRaw = qubesAdminEvents "admin.Events"

qubesEvents :: Producer QubesEvent (P.SafeT IO) ()
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

qubesListVMs :: IO (Map.Map BL.ByteString QubesVMInfo)
qubesListVMs = qubesAdminCall "admin.vm.List" >>= fromOk >>= parse
  where
    fromOk (Ok x) = return x
    fromOk x = fail $ "unexpected reply: " <> show x
    parse reply = BLC.split '\n' reply
      & filter (/="")
      & map parseLine
      & Map.fromList
      & return
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

qubesGetProperty :: BL.ByteString -> IO (Bool, BL.ByteString, BL.ByteString)
qubesGetProperty name = qubesAdminCall ("admin.property.Get+" <> name) >>= fromOk >>= parse
  where
    fromOk (Ok x) = return x
    fromOk x = fail $ "unexpected reply: " <> show x
    parse reply = return (isDefault == "default=True", BL.drop 5 typeStr, value)
      where
        splitOn ch = fmap BLC.tail . BLC.break (==ch)
        (isDefault, (typeStr, value)) = splitOn ' ' reply & fmap (splitOn ' ')

qubesGetDefaultPool :: IO BL.ByteString
qubesGetDefaultPool = third <$> qubesGetProperty "default_pool"
  where third (_, _, x) = x

qubesGetPoolInfo :: BL.ByteString -> IO [(BL.ByteString, BL.ByteString)]
qubesGetPoolInfo name = qubesAdminCall ("admin.pool.Info+" <> name) >>= fromOk >>= parse
  where
    fromOk (Ok x) = return x
    fromOk x = fail $ "unexpected reply: " <> show x
    parse reply = BLC.split '\n' reply
      & filter (/="")
      & map parseLine
      & return
    parseLine = fmap BLC.tail . BLC.break (=='=')

qubesUsageOfDefaultPool :: IO (Maybe Int, Maybe Int)
qubesUsageOfDefaultPool = qubesGetDefaultPool >>= qubesGetPoolInfo >>= extract
  where
    extract props = return (tryReadProp "usage" props, tryReadProp "size" props)
    tryReadProp :: Read a => BL.ByteString -> [(BL.ByteString, BL.ByteString)] -> Maybe a
    tryReadProp name props = readMaybe . BLC.unpack =<< lookup name props
