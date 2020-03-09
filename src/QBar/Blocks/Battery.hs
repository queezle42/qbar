{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}


module QBar.Blocks.Battery where


import QBar.Core
import QBar.Blocks.Utils
import QBar.BlockOutput

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO

import System.Directory
import Data.Maybe

import Control.Lens


data BatteryStatus = BatteryCharging | BatteryDischarging | BatteryOther
  deriving (Show)


data BatteryState = BatteryState
  { _status :: BatteryStatus
  , _powerNow :: Maybe Int
  , _energyNow :: Int
  , _energyFull :: Int
} deriving (Show)


getBatteryState :: FilePath -> IO (Maybe BatteryState)
getBatteryState path = maybe getBatteryStateCharge (return . Just) =<< getBatteryStateEnergy
  where
    getVoltage :: IO Double
    getVoltage = readIO =<< readFile (path <> "/voltage_now")
    getBatteryStateEnergy :: IO (Maybe BatteryState)
    getBatteryStateEnergy = tryMaybe $ do
      status' <- batteryStatus
      energyNow' <- readIO =<< readFile (path <> "/energy_now")
      energyFull' <- readIO =<< readFile (path <> "/energy_full")
      powerNow' <- batteryPower getVoltage
      return BatteryState
        { _status = status'
        , _powerNow = powerNow'
        , _energyNow = energyNow'
        , _energyFull = energyFull'
        }
    getBatteryStateCharge :: IO (Maybe BatteryState)
    getBatteryStateCharge = tryMaybe $ do
      status' <- batteryStatus
      voltageNow' <- getVoltage
      powerNow' <- batteryPower (return voltageNow')
      chargeNow' <- readIO =<< readFile (path <> "/charge_now")
      chargeFull' <- readIO =<< readFile (path <> "/charge_full")
      return BatteryState
        { _status = status'
        , _powerNow = powerNow'
        , _energyNow = round $ voltageNow' * chargeNow' / 1000000
        , _energyFull = round $ voltageNow' * chargeFull' / 1000000
        }
    batteryPower :: IO Double -> IO (Maybe Int)
    batteryPower getVoltage' = do
      power' <- tryMaybe $ readIO =<< readFile (path <> "/power_now")
      case power' of
        power@(Just _) -> return power
        Nothing -> tryMaybe $ do
          current <- readIO =<< readFile (path <> "/current_now")
          voltage <- getVoltage'
          return $ round $ voltage * current / 1000000
    batteryStatus :: IO BatteryStatus
    batteryStatus = do
      statusText <- tryMaybe $ T.strip <$> TIO.readFile (path <> "/status")
      return $ if
        | statusText == Just "Charging" -> BatteryCharging
        | statusText == Just "Discharging" -> BatteryDischarging
        | otherwise -> BatteryOther


batteryBlock :: Block
batteryBlock = pullBlock $ forever $ do
  batteryPaths <- liftIO $ map ((apiPath <> "/") <>) . filter (T.isPrefixOf "BAT" . T.pack) <$> getDirectoryContents apiPath
  batteryStates <- liftIO $ mapM getBatteryState batteryPaths
  isPlugged <- liftIO getPluggedState
  updateBatteryBlock isPlugged $ catMaybes batteryStates
  where
    apiPath :: FilePath
    apiPath = "/sys/class/power_supply"

    getPluggedState :: IO Bool
    getPluggedState = or . catMaybes <$> mapM getPluggedStateFromFile powerSupplyPaths
      where
        powerSupplyPaths :: [FilePath]
        powerSupplyPaths = ["/sys/class/power_supply/AC/online", "/sys/class/power_supply/ACAD/online"]
        getPluggedStateFromFile :: FilePath -> IO (Maybe Bool)
        getPluggedStateFromFile f = do
          line <- tryMaybe $ T.strip <$> TIO.readFile f
          case line of
            Just "1" -> return . return $ True
            _ -> return . return $ False


updateBatteryBlock :: Bool -> [BatteryState] -> PullBlock' ()
updateBatteryBlock _ [] = sendEmptyBlockUpdate
updateBatteryBlock isPlugged bs = sendBlockUpdate $ (shortText.~shortText') $ mkBlockOutput fullText'
  where
    fullText' :: BlockText
    fullText' = overallPercentage <> optionalEachBattery <> optionalOverallEstimate

    shortText' :: Maybe BlockText
    shortText' = Just overallPercentage

    batteryIcon :: T.Text
    batteryIcon
      | isPlugged = "🔌\xFE0E"
      | otherwise = "🔋\xFE0E"

    optionalEachBattery :: BlockText
    optionalEachBattery
      | length bs < 2 = mempty
      | otherwise = normalText " " <> eachBattery

    eachBattery :: BlockText
    eachBattery = surroundWith normalText "[" "]" $ (intercalate (normalText ", ") . map perSingleBattery) bs

    perSingleBatteryArrow :: BatteryState -> T.Text
    perSingleBatteryArrow b
      | BatteryCharging <- _status b = "▲"
      | BatteryDischarging <- _status b = "▼"
      | otherwise = T.empty

    perSingleBattery :: BatteryState -> BlockText
    perSingleBattery b = importantText (batteryImportance [b]) $ perSingleBatteryArrow b <> (formatFloatN 0 . batteryPercentage) [b] <> "%"

    overallPercentage :: BlockText
    overallPercentage = mkText (not isPlugged) (batteryImportance bs) $ batteryIcon <> " " <> (formatFloatN 0 . batteryPercentage $ bs) <> "%"

    optionalOverallEstimate :: BlockText
    optionalOverallEstimate = maybe mempty (surroundWith normalText " (" ")") . batteryEstimateFormated $ bs


batteryImportance :: [BatteryState] -> Importance
batteryImportance = toImportance (0, 60, 80, 90, 100) . (100 -) . batteryPercentage


batteryPercentage :: [BatteryState] -> Float
batteryPercentage batteryStates
  | batteryEnergyFull == 0 = 0
  | otherwise = batteryEnergyNow * 100 / batteryEnergyFull
  where
    batteryEnergyFull :: Float
    batteryEnergyFull = fromIntegral . sum . map _energyFull $ batteryStates

    batteryEnergyNow :: Float
    batteryEnergyNow = fromIntegral . sum . map _energyNow $ batteryStates


batteryEstimateFormated :: [BatteryState] -> Maybe BlockText
batteryEstimateFormated batteryStates = do
  allSeconds <- batteryEstimate batteryStates
  let allMinutes = div allSeconds 60
  let allHours = div allMinutes 60
  let minutes = allMinutes - allHours * 60
  return $ normalText $ (T.pack . show $ allHours) <> ":" <> (T.justifyRight 2 '0' . T.pack . show $ minutes)


batteryIsCharging :: [BatteryState] -> Bool
batteryIsCharging = any (singleBatteryIsCharging . _status)
  where
    singleBatteryIsCharging :: BatteryStatus -> Bool
    singleBatteryIsCharging BatteryCharging = True
    singleBatteryIsCharging _ = False


batteryIsDischarging :: [BatteryState] -> Bool
batteryIsDischarging = any (singleBatteryIsDischarging . _status)
  where
    singleBatteryIsDischarging :: BatteryStatus -> Bool
    singleBatteryIsDischarging BatteryDischarging = True
    singleBatteryIsDischarging _ = False


batteryEstimate :: [BatteryState] -> Maybe Int
batteryEstimate batteryStates
  | batteryPowerNow == 0 = Nothing
  | isCharging, not isDischarging = ensure (>0) batteryEstimateCharging
  | isDischarging, not isCharging = ensure (>0) batteryEstimateDischarging
  | otherwise = Nothing
  where
    isCharging :: Bool
    isCharging = batteryIsCharging batteryStates

    isDischarging :: Bool
    isDischarging = batteryIsDischarging batteryStates

    batteryPowerNow :: Int
    batteryPowerNow = sum . mapMaybe _powerNow $ batteryStates

    batteryEnergyNow :: Int
    batteryEnergyNow = sum . map _energyNow $ batteryStates

    batteryEnergyFull :: Int
    batteryEnergyFull = sum . map _energyFull $ batteryStates

    batteryEstimateCharging :: Int
    batteryEstimateCharging = div ((batteryEnergyFull - batteryEnergyNow) * 3600) batteryPowerNow

    batteryEstimateDischarging :: Int
    batteryEstimateDischarging = div (batteryEnergyNow * 3600) batteryPowerNow
