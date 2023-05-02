module QBar.Blocks.Squeekboard (
  squeekboardBlock,
) where

import Control.Monad.Except (MonadError)
import Data.Either (isRight)
import qualified DBus
import qualified DBus.Client as DBus
import DBus.Internal.Message (signalBody)
import Pipes.Core
import QBar.BlockHelper
import QBar.BlockOutput
import QBar.Blocks.NetworkManager (getDBusProperty, runExceptT_)
import QBar.Core
import QBar.Prelude

squeekboardBlock :: Bool -> Block
squeekboardBlock autoHide = runSignalBlockConfiguration $ SignalBlockConfiguration {
  aquire,
  release,
  signalThread = Nothing,
  signalBlock = networkManagerBlock',
  interval = Nothing
}
  where
    aquire :: (() -> IO ()) -> BarIO DBus.Client
    aquire trigger = liftIO $ do
      client <- DBus.connectSession
      let matchRule = DBus.matchAny {
        DBus.matchPath = Just "/sm/puri/OSK0",
        DBus.matchInterface = Just "org.freedesktop.DBus.Properties",
        DBus.matchMember = Just "PropertiesChanged"
      }
      void . DBus.addMatch client matchRule $ dbusSignalHandler trigger
      let matchRule2 = DBus.matchAny {
        DBus.matchSender = Just "org.freedesktop.DBus",
        DBus.matchPath = Just "/org/freedesktop/DBus",
        DBus.matchInterface = Just "org.freedesktop.DBus",
        DBus.matchMember = Just "NameOwnerChanged"
      }
      void . DBus.addMatch client matchRule2 $ \signal -> when (nameOwnerChangedIsPuriOSK0 (signalBody signal)) $ dbusSignalHandler trigger signal
      return client
    nameOwnerChangedIsPuriOSK0 :: [DBus.Variant] -> Bool
    nameOwnerChangedIsPuriOSK0 (path:_) = path == DBus.toVariant ("sm.puri.OSK0" :: String)
    nameOwnerChangedIsPuriOSK0 _ = False
    release :: DBus.Client -> BarIO ()
    release = liftIO . DBus.disconnect
    networkManagerBlock' :: DBus.Client -> SignalBlock ()
    networkManagerBlock' client = (liftBarIO . networkManagerBlock'' client) >=> respond >=> networkManagerBlock' client
    networkManagerBlock'' :: DBus.Client -> Signal () -> BarIO (Maybe BlockOutput)
    networkManagerBlock'' client (EventSignal Click{button=1}) = do
      mCurrent <- runExceptT_ (getVisible client)
      case mCurrent of
        (Just current) -> void $ setVisible client (not current)
        Nothing -> return ()
      networkManagerBlock''' client
    networkManagerBlock'' client _ = networkManagerBlock''' client
    networkManagerBlock''' :: DBus.Client -> BarIO (Maybe BlockOutput)
    networkManagerBlock''' client = blockOutput <$> runExceptT_ (getVisible client)
    blockOutput :: Maybe Bool -> Maybe BlockOutput
    blockOutput (Just isEnabled) = Just (mkBlockOutput (mkText isEnabled normalImportant "⌨\xFE0E osk"))
    blockOutput Nothing = if autoHide then Nothing else Just (mkBlockOutput $ mkText False errorImportant "⌨\xFE0E n/a")
    dbusSignalHandler :: (() -> IO ()) -> DBus.Signal -> IO ()
    dbusSignalHandler trigger _signal = trigger ()

getVisible :: (MonadError () m, MonadIO m) => DBus.Client -> m Bool
getVisible client = do
  getDBusProperty
    client
    "sm.puri.OSK0"
    "/sm/puri/OSK0"
    "sm.puri.OSK0"
    "Visible"

setVisible :: (MonadIO m) => DBus.Client -> Bool -> m Bool
setVisible client value = do
  let methodCall = ((DBus.methodCall "/sm/puri/OSK0" "sm.puri.OSK0" "SetVisible") {DBus.methodCallDestination = Just "sm.puri.OSK0", DBus.methodCallBody = [DBus.toVariant value]})
  isRight <$> liftIO (DBus.call client methodCall)
