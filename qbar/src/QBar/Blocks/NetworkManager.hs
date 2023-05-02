module QBar.Blocks.NetworkManager (
  getDBusProperty,
  networkManagerBlock,
  runExceptT_,
) where

import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import qualified DBus
import qualified DBus.Client as DBus
import qualified Data.Map as Map
import qualified Data.Text.Lazy as T
import Data.Word (Word32, Word8)
import QBar.BlockHelper
import QBar.BlockOutput
import QBar.Blocks.Utils
import QBar.Core
import QBar.Prelude

data ConnectionInfo = WifiConnection Text Int | WwanConnection Text Int | EthernetConnection Text
  deriving (Show)

fromJust :: MonadError () m => Maybe a -> m a
fromJust (Just a) = return a
fromJust Nothing = throwError ()

runExceptT_ :: Monad m => ExceptT () m a -> m (Maybe a)
runExceptT_ a = either (const Nothing) Just <$> runExceptT a

getDBusProperty :: (MonadError () m, MonadIO m, DBus.IsVariant a) => DBus.Client -> DBus.BusName -> DBus.ObjectPath -> DBus.InterfaceName -> DBus.MemberName -> m a
getDBusProperty client busName objectPath interfaceName memberName = do
  result' <- tryMaybe' $ do
    let methodCall = ((DBus.methodCall objectPath interfaceName memberName) {DBus.methodCallDestination = Just busName})
    result <- either (const Nothing) Just <$> DBus.getProperty client methodCall
    return $ DBus.fromVariant =<< result
  fromJust result'

getConnectionInfo :: (MonadError () m, MonadIO m) => DBus.Client -> DBus.ObjectPath -> m ConnectionInfo
getConnectionInfo client connectionObjectPath = do
  connectionType <- getActiveConnectionType client connectionObjectPath
  connectionName <- getActiveConnectionName client connectionObjectPath
  case connectionType of
    "802-11-wireless" -> do
      devices <- getActiveConnectionDevice client connectionObjectPath
      device <- fromJust $ listToMaybe devices
      accessPoint <- getDeviceAccessPoint client device
      signalStrength <- fromIntegral <$> getAccessPointSignalStrength client accessPoint
      return $ WifiConnection connectionName signalStrength
    "gsm" -> do
      devices <- getActiveConnectionDevice client connectionObjectPath
      device <- fromJust $ listToMaybe devices
      udi <- getDeviceUdi client device
      signalQuality <- getModemSignalQuality client $ DBus.objectPath_ udi
      return $ WwanConnection connectionName . fromIntegral . fst $ signalQuality
    "802-3-ethernet" -> return $ EthernetConnection connectionName
    _ -> throwError ()

networkManagerBlock :: Block
networkManagerBlock = runSignalBlockConfiguration $ SignalBlockConfiguration {
  aquire,
  release,
  signalThread = Nothing,
  signalBlock = networkManagerBlock',
  interval = Just defaultInterval
}
  where
    aquire :: (() -> IO ()) -> BarIO DBus.Client
    aquire trigger = liftIO $ do
      client <- DBus.connectSystem
      let matchRule = DBus.matchAny {
        DBus.matchPath = Just "/org/freedesktop/NetworkManager",
        DBus.matchInterface = Just "org.freedesktop.DBus.Properties"
      }
      void . DBus.addMatch client matchRule $ dbusSignalHandler trigger
      return client
    release :: DBus.Client -> BarIO ()
    release = liftIO . DBus.disconnect
    networkManagerBlock' :: DBus.Client -> SignalBlock ()
    networkManagerBlock' client = (liftBarIO . networkManagerBlock'' client) >=> respondBlockUpdate >=> networkManagerBlock' client
    networkManagerBlock'' :: DBus.Client -> Signal () -> BarIO BlockOutput
    networkManagerBlock'' client _ = do
      primaryConnection <- runExceptT_ $ getPrimaryConnectionPath client
      primaryConnectionInfo <- case primaryConnection of
        Just primaryConnection' -> runExceptT_ $ getConnectionInfo client primaryConnection'
        Nothing -> return Nothing
      return . mkBlockOutput $ fullText' primaryConnectionInfo
    fullText' :: Maybe ConnectionInfo -> BlockText
    fullText' connectionInfo = fullText'' connectionInfo
      where
        importanceLevel :: Importance
        importanceLevel = case connectionInfo of
          Nothing -> errorImportant
          (Just (WifiConnection _ strength)) -> toImportance (0, 85 ,100, 100, 100) . (100 -) $ strength
          (Just (WwanConnection _ strength)) -> toImportance (0, 85 ,100, 100, 100) . (100 -) $ strength
          (Just (EthernetConnection _)) -> normalImportant
        fullText'' :: Maybe ConnectionInfo -> BlockText
        fullText'' Nothing = importantText importanceLevel "❌\xFE0E No Connection"
        fullText'' (Just (WifiConnection name strength)) = importantText importanceLevel $ "📡\xFE0E " <> name <> " " <> formatPercent strength
        fullText'' (Just (WwanConnection _ signalQuality)) = importantText importanceLevel $ "📶\xFE0E " <> formatPercent signalQuality
        fullText'' (Just (EthernetConnection _)) = importantText importanceLevel "🔌\xFE0E Ethernet"
    dbusSignalHandler :: (() -> IO ()) -> DBus.Signal -> IO ()
    dbusSignalHandler trigger signal = when (primaryConnectionHasChanged signal) $ trigger ()
    primaryConnectionHasChanged :: DBus.Signal -> Bool
    primaryConnectionHasChanged = any (maybe False (containsKey "PrimaryConnection") . DBus.fromVariant) . DBus.signalBody
    containsKey :: String -> Map.Map String DBus.Variant -> Bool
    containsKey = Map.member

formatPercent :: Int -> Text
formatPercent a = T.justifyRight 3 ' ' $ (T.pack . show . max 0 . min 100) a <> "%"

getPrimaryConnectionPath :: (MonadError () m, MonadIO m) => DBus.Client -> m DBus.ObjectPath
getPrimaryConnectionPath client =
  getDBusProperty
    client
    "org.freedesktop.NetworkManager"
    "/org/freedesktop/NetworkManager"
    "org.freedesktop.NetworkManager"
    "PrimaryConnection"

getActiveConnectionType :: (MonadError () m, MonadIO m) => DBus.Client -> DBus.ObjectPath -> m Text
getActiveConnectionType client objectPath =
  getDBusProperty
    client
    "org.freedesktop.NetworkManager"
    objectPath
    "org.freedesktop.NetworkManager.Connection.Active"
    "Type"

getActiveConnectionName :: (MonadError () m, MonadIO m) => DBus.Client -> DBus.ObjectPath -> m Text
getActiveConnectionName client objectPath =
  getDBusProperty
    client
    "org.freedesktop.NetworkManager"
    objectPath
    "org.freedesktop.NetworkManager.Connection.Active"
    "Id"

getActiveConnectionDevice :: (MonadError () m, MonadIO m) => DBus.Client -> DBus.ObjectPath -> m [DBus.ObjectPath]
getActiveConnectionDevice client objectPath =
  getDBusProperty
    client
    "org.freedesktop.NetworkManager"
    objectPath
    "org.freedesktop.NetworkManager.Connection.Active"
    "Devices"

getDeviceAccessPoint :: (MonadError () m, MonadIO m) => DBus.Client -> DBus.ObjectPath -> m DBus.ObjectPath
getDeviceAccessPoint client objectPath =
  getDBusProperty
    client
    "org.freedesktop.NetworkManager"
    objectPath
    "org.freedesktop.NetworkManager.Device.Wireless"
    "ActiveAccessPoint"

getDeviceUdi :: (MonadError () m, MonadIO m) => DBus.Client -> DBus.ObjectPath -> m String
getDeviceUdi client objectPath =
  getDBusProperty
    client
    "org.freedesktop.NetworkManager"
    objectPath
    "org.freedesktop.NetworkManager.Device"
    "Udi"

getAccessPointSignalStrength :: (MonadError () m, MonadIO m) => DBus.Client -> DBus.ObjectPath -> m Word8
getAccessPointSignalStrength client objectPath =
  getDBusProperty
    client
    "org.freedesktop.NetworkManager"
    objectPath
    "org.freedesktop.NetworkManager.AccessPoint"
    "Strength"

getModemSignalQuality :: (MonadError () m, MonadIO m) => DBus.Client -> DBus.ObjectPath -> m (Word32, Bool)
getModemSignalQuality client objectPath =
  getDBusProperty
    client
    "org.freedesktop.ModemManager1"
    objectPath
    "org.freedesktop.ModemManager1.Modem"
    "SignalQuality"
