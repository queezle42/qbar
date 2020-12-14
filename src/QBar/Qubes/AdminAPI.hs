module QBar.Qubes.AdminAPI where

import Control.Monad (forM)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Network.HostName
import Pipes
import qualified Pipes.Safe as P
import System.IO (Handle, hSetBinaryMode)
import System.Process.Typed

data QubesAdminReturn
  = Ok { okContent :: BL.ByteString }
  | Event { evSubject :: BL.ByteString, evEvent :: BL.ByteString, evProperties :: [(BL.ByteString, BL.ByteString)] }
  | Exception { excType :: BL.ByteString, excTraceback :: BL.ByteString, excFormatString :: BL.ByteString, excFields :: [BL.ByteString] }
  deriving (Eq, Ord, Show, Read)

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
    forM evProperties $ \(k, v) -> do
      putLazyByteStringNul k
      putLazyByteStringNul v
    putWord8 0x00
  put Exception {excType, excTraceback, excFormatString, excFields} = do
    putWord8 0x32 >> putWord8 0x00
    putLazyByteStringNul excType
    putLazyByteStringNul excTraceback
    putLazyByteStringNul excFormatString
    forM excFields putLazyByteStringNul
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

qubesAdminConnect :: String -> IO (Process () Handle ())
qubesAdminConnect serviceName = do
  hostname <- getHostName
  let cmd = if hostname == "dom0"
      then "qubesd-query dom0 " <> serviceName <> " dom0"
      else "qrexec-client-vm dom0 " <> serviceName
  --NOTE qubesd-query and qrexec-client-vm don't like it if their input
  --     is closed rather than empty.
  --     hangs: qrexec-client-vm dom0 admin.vm.List <&-
  --     works: qrexec-client-vm dom0 admin.vm.List </dev/null
  let processConfig = setStdin nullStream $ setStdout createPipe $ shell cmd
  startProcess processConfig

qubesAdminCall :: String -> IO QubesAdminReturn
qubesAdminCall serviceName = do
  process <- qubesAdminConnect serviceName
  let stdout = getStdout process
  hSetBinaryMode stdout True
  reply <- decode <$> BL.hGetContents stdout
  case reply of
    Ok {} -> return reply
    Exception {} -> return reply
    Event {} -> fail "service has returned events instead of a reply"

qubesAdminCallP :: String -> Producer QubesAdminReturn (P.SafeT IO) ()
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

qubesAdminEvents :: String -> Producer QubesAdminReturn (P.SafeT IO) ()
qubesAdminEvents serviceName = qubesAdminCallP serviceName >-> onlyEvents
  where
    onlyEvents :: Pipe QubesAdminReturn QubesAdminReturn (P.SafeT IO) ()
    onlyEvents = forever $ await >>= \reply -> case reply of
        Ok {} -> fail "service has returned OK instead of events"
        Exception {} -> fail $ "service has returned an exception: " ++ show reply
        Event {} -> yield reply

qubesVMStats :: Producer QubesAdminReturn (P.SafeT IO) ()
qubesVMStats = qubesAdminEvents "admin.vm.Stats"

printEvents  :: Show a => Producer a (P.SafeT IO) () -> IO ()
printEvents prod = P.runSafeT $ runEffect $ prod >-> (forever $ await >>= liftIO . print)
