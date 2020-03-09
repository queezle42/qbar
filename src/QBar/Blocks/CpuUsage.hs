{-# LANGUAGE TemplateHaskell #-}

module QBar.Blocks.CpuUsage where

import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad.State (StateT, evalStateT, lift)
import qualified Data.Attoparsec.Text.Lazy as AT
import qualified Data.Text.Lazy as T
import QBar.BlockOutput
import QBar.Blocks.Utils
import QBar.Core

{-
  For time accounting the guest fields need to be ignored according to the kernel source code
  (https://git.kernel.org/pub/scm/linux/kernel/git/stable/linux.git/tree/kernel/sched/cputime.c#n150)
  the accounting also counts the guest time to user or nice respectively so applications
  that are not aware of the new fields do not loose time.
-}
data CpuStat
  = CpuStat
      { userTime :: Int,
        niceTime :: Int,
        systemTime :: Int,
        idleTime :: Int,
        iowaitTime :: Int,
        irqTime :: Int,
        softirqTime :: Int,
        stealTime :: Int
      }
  deriving (Show)

getCpuStat :: IO (Maybe CpuStat)
getCpuStat = parseFile "/proc/stat" cpuStat
  where
    skipLine :: AT.Parser ()
    skipLine = AT.skipWhile (not . AT.isEndOfLine) *> AT.skipWhile AT.isEndOfLine
    cpuStat :: AT.Parser CpuStat
    cpuStat = cpuLine <|> (skipLine *> cpuStat)
    cpuLine :: AT.Parser CpuStat
    cpuLine = AT.string "cpu " *> do
      userTime' <- AT.skipSpace *> AT.decimal
      niceTime' <- AT.skipSpace *> AT.decimal
      systemTime' <- AT.skipSpace *> AT.decimal
      idleTime' <- AT.skipSpace *> AT.decimal
      iowaitTime' <- AT.skipSpace *> AT.decimal
      irqTime' <- AT.skipSpace *> AT.decimal
      softirqTime' <- AT.skipSpace *> AT.decimal
      stealTime' <- AT.skipSpace *> AT.decimal
      return $
        CpuStat
          { userTime = userTime',
            niceTime = niceTime',
            systemTime = systemTime',
            idleTime = idleTime',
            iowaitTime = iowaitTime',
            irqTime = irqTime',
            softirqTime = softirqTime',
            stealTime = stealTime'
          }

differenceCpuStat :: CpuStat -> CpuStat -> CpuStat
differenceCpuStat a b =
  CpuStat
    { userTime = userTime a - userTime b,
      niceTime = niceTime a - niceTime b,
      systemTime = systemTime a - systemTime b,
      idleTime = idleTime a - idleTime b,
      iowaitTime = iowaitTime a - iowaitTime b,
      irqTime = irqTime a - irqTime b,
      softirqTime = softirqTime a - softirqTime b,
      stealTime = stealTime a - stealTime b
    }

cpuTotalTime :: Num a => CpuStat -> a
cpuTotalTime
  CpuStat
    { userTime,
      niceTime,
      systemTime,
      idleTime,
      iowaitTime,
      irqTime,
      softirqTime,
      stealTime
    } =
    fromIntegral . sum $
      [ userTime,
        niceTime,
        systemTime,
        idleTime,
        iowaitTime,
        irqTime,
        softirqTime,
        stealTime
      ]

cpuUsage :: CpuStat -> Float
cpuUsage stat@CpuStat {idleTime, iowaitTime} = 1 - (totalIdleTime / totalTime)
  where
    totalTime :: Num a => a
    totalTime = cpuTotalTime stat
    totalIdleTime :: Num a => a
    totalIdleTime = fromIntegral $ idleTime + iowaitTime

data CpuBlockState
  = CpuBlockState
      { _lastCpuStat :: CpuStat,
        _lastCpuUsage :: Float
      }
  deriving (Show)

makeLenses ''CpuBlockState

cpuUsageBlock :: Int -> Block
cpuUsageBlock decimalPlaces = pullBlock $ evalStateT cpuUsageBlock' createState
  where
    cpuUsageBlock' :: StateT CpuBlockState PullBlock' ExitBlock
    cpuUsageBlock' = forever $ do
      updateState
      importance <- cpuUsageImportance
      text <- ("💻\xFE0E " <>) <$> cpuUsageText
      lift $ sendBlockUpdate $ mkBlockOutput $ importantText importance text
    createState :: CpuBlockState
    createState =
      CpuBlockState
        { _lastCpuStat = CpuStat 0 0 0 0 0 0 0 0,
          _lastCpuUsage = 0
        }
    cpuUsageImportance :: Monad m => StateT CpuBlockState m Importance
    cpuUsageImportance = toImportance (0, 60, 80, 90 ,100) <$> use lastCpuUsage
    cpuUsageTextWidth :: Num a => a
    cpuUsageTextWidth
      | decimalPlaces == 0 = 3
      | otherwise = fromIntegral $ 4 + decimalPlaces
    cpuUsageText :: Monad m => StateT CpuBlockState m Text
    cpuUsageText = do
      cpuUsage' <- use lastCpuUsage
      let cpuUsageText' = formatFloatN decimalPlaces cpuUsage' <> "%"
      if T.length cpuUsageText' <= cpuUsageTextWidth
        then return $ T.justifyRight cpuUsageTextWidth ' ' cpuUsageText'
        else return $ "99" <> (if decimalPlaces == 0 then "" else "." <> T.replicate (fromIntegral decimalPlaces) "9") <> "%"
    updateState :: MonadBarIO m => StateT CpuBlockState m ()
    updateState = do
      oldCpuStat <- use lastCpuStat
      newCpuStat' <- liftIO getCpuStat
      case newCpuStat' of
        Nothing -> return ()
        (Just newCpuStat) -> do
          let cpuStatDiff = differenceCpuStat newCpuStat oldCpuStat
          lastCpuUsage .= 100 * cpuUsage cpuStatDiff
          lastCpuStat .= newCpuStat
