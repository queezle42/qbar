{-# LANGUAGE ScopedTypeVariables #-}


module QBar.Blocks.Battery where


import QBar.Core hiding (name)
import QBar.BlockText
import Pipes

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO

import System.Directory
import Control.Exception (catch, IOException)
import Data.Maybe
import Text.Read (readMaybe)
import Numeric (showFFloat)

import Control.Lens


formatFloatN :: RealFloat a => Int -> a -> T.Text
formatFloatN n f = T.pack $ showFFloat (Just n) f ""


data BatteryStatus = BatteryCharging | BatteryDischarging | BatteryOther
  deriving (Show)


data BatteryState = BatteryState
  { _status :: BatteryStatus
  , _powerNow :: Maybe Int
  , _energyNow :: Int
  , _energyFull :: Int
} deriving (Show)


tryMaybe :: IO a -> IO (Maybe a)
tryMaybe a = catch (Just <$> a) (\ (_ :: IOException) -> return Nothing)


getBatteryState :: FilePath -> IO (Maybe BatteryState)
getBatteryState path = tryMaybe $ do
  status' <- TIO.readFile (path <> "/status")
  powerNow' <- tryMaybe $ TIO.readFile (path <> "/power_now")
  energyNow' <- readIO =<< readFile (path <> "/energy_now")
  energyFull' <- readIO =<< readFile (path <> "/energy_full")
  return BatteryState
    { _status = batteryStatus . T.strip $ status'
    , _powerNow = readMaybe . T.unpack =<< powerNow'
    , _energyNow = energyNow'
    , _energyFull = energyFull'
    }
  where
    batteryStatus :: T.Text -> BatteryStatus
    batteryStatus statusText
      | statusText == "Charging" = BatteryCharging
      | statusText == "Discharging" = BatteryDischarging
      | otherwise = BatteryOther


batteryBlock :: PullBlock
batteryBlock = do
  batteryPaths <- liftIO $ map ((apiPath <> "/") <>) . filter (T.isPrefixOf "BAT" . T.pack) <$> getDirectoryContents apiPath
  batteryStates <- liftIO $ mapM getBatteryState batteryPaths
  isPlugged <- liftIO getPluggedState
  yield $ fromMaybe emptyBlock (batteryBlockOutput isPlugged $ catMaybes batteryStates)
  batteryBlock
  where
    apiPath :: FilePath
    apiPath = "/sys/class/power_supply"

    getPluggedState :: IO Bool
    getPluggedState = do
      line <- tryMaybe $ T.strip <$> TIO.readFile "/sys/class/power_supply/AC/online"
      case line of
        Just "1" -> return True
        _ -> return False


batteryBlockOutput :: Bool -> [BatteryState] -> Maybe BlockOutput
batteryBlockOutput isPlugged bs = (shortText.~shortText') . createBlock <$> fullText'
  where
    fullText' :: Maybe BlockText
    fullText'
      | null bs = Nothing
      | otherwise = Just $ normalText (batteryIcon <> " ") <> overallPercentage <> optionalEachBattery <> optionalOverallEstimate

    shortText' :: Maybe BlockText
      | null bs = Nothing
      | otherwise = Just $ normalText (batteryIcon <> " ") <> overallPercentage

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
      | BatteryCharging <- _status b = "⬆"
      | BatteryDischarging <- _status b = "⬇"
      | otherwise = T.empty

    perSingleBattery :: BatteryState -> BlockText
    perSingleBattery b = importantText (batteryImportance bs) $ perSingleBatteryArrow b <> (formatFloatN 0 . batteryPercentage) [b] <> "%"

    overallPercentage :: BlockText
    overallPercentage = activeImportantText (batteryImportance bs) $ (formatFloatN 0 . batteryPercentage $ bs) <> "%"

    optionalOverallEstimate :: BlockText
    optionalOverallEstimate = maybe mempty (\s -> surroundWith normalText " (" ")" s) . batteryEstimateFormated $ bs


batteryImportance :: [BatteryState] -> Importance
batteryImportance batteryStates
    | percentage < 10 = 4 -  percentage       / 10
    | percentage < 35 = 3 - (percentage - 10) / 25
    | percentage < 75 = 2 - (percentage - 35) / 40
    | otherwise       = 1 - (percentage - 75) / 25
  where
    percentage :: Float
    percentage = batteryPercentage batteryStates


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
  | isCharging, not isDischarging = Just $ div ((batteryEnergyFull - batteryEnergyNow) * 3600) batteryPowerNow
  | isDischarging, not isCharging = Just $ div (batteryEnergyNow * 3600) batteryPowerNow
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
