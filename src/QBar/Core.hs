{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module QBar.Core where

import QBar.BlockOutput
import QBar.Time
import QBar.Util

import Control.Concurrent.Async
import Control.Concurrent.Event as Event
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan (TChan, writeTChan)
import Control.Lens
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT)
import Data.Aeson.TH
import Data.Either (isRight)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as T
import Pipes
import Pipes.Concurrent
import Pipes.Safe (SafeT, runSafeT)
import qualified Pipes.Prelude as PP

data MainOptions = MainOptions {
  verbose :: Bool,
  indicator :: Bool,
  socketLocation :: Maybe T.Text
}

data BlockEvent = Click {
  name :: T.Text,
  button :: Int
} deriving Show
$(deriveJSON defaultOptions ''BlockEvent)


data PushMode = PushMode
data PullMode = PullMode
data CachedMode = CachedMode


type BlockEventHandler = BlockEvent -> BarIO ()

type BlockState = Maybe (BlockOutput, Maybe BlockEventHandler)
data BlockUpdateReason = DefaultUpdate | PullUpdate | UserUpdate
type BlockUpdate = (BlockState, BlockUpdateReason)

type Block = Producer BlockUpdate BarIO


-- |Block that 'yield's an update whenever the block should be changed
type PushBlock = Block PushMode
-- |Block that generates an update on 'yield'. Should only be pulled when an update is required.
type PullBlock = Block PullMode

-- |Cache that holds multiple BlockStates. When iterated it always immediately 'yield's the latest update, so it should only be pulled when a bar update has been requested.
type BlockCache = Producer [BlockState] BarIO CachedMode

class IsCachable a where
  toCachedBlock :: a -> BlockCache

instance IsCachable PushBlock where
  toCachedBlock = cachePushBlock
instance IsCachable PullBlock where
  toCachedBlock = cachePushBlock . schedulePullBlock
instance IsCachable BlockCache where
  toCachedBlock = id

class IsBlockMode a where
  exitBlock :: Block a
instance IsBlockMode PushMode where
  exitBlock = return PushMode
instance IsBlockMode PullMode where
  exitBlock = return PullMode

exitCache :: BlockCache
exitCache = return CachedMode


type BarIO = SafeT (ReaderT Bar IO)

data Bar = Bar {
  requestBarUpdate :: BlockUpdateReason -> IO (),
  newBlockChan :: TChan BlockCache,
  barSleepScheduler :: SleepScheduler
}
instance HasSleepScheduler BarIO where
  askSleepScheduler = barSleepScheduler <$> askBar
instance HasSleepScheduler (Proxy a' a b' b BarIO) where
  askSleepScheduler = lift askSleepScheduler


newtype BarUpdateChannel = BarUpdateChannel (IO ())
type BarUpdateEvent = Event.Event


class (MonadIO m) => MonadBarIO m where
  liftBarIO :: BarIO a -> m a
instance MonadBarIO BarIO where
  liftBarIO = id
instance (MonadBarIO m) => MonadBarIO (Proxy a' a b' b m) where
  liftBarIO = lift . liftBarIO
instance (MonadBarIO m) => MonadBarIO (StateT a m) where
  liftBarIO = lift . liftBarIO
instance (MonadBarIO m) => MonadBarIO (ReaderT a m) where
  liftBarIO = lift . liftBarIO
instance (MonadBarIO m, Monoid a) => MonadBarIO (WriterT a m) where
  liftBarIO = lift . liftBarIO

askBar :: MonadBarIO m => m Bar
askBar = liftBarIO $ lift ask


class (MonadBarIO m) => MonadBlock m where
  liftBlock :: Block a -> m a
instance MonadBlock Block where
  liftBlock = id
instance (MonadBlock m) => MonadBlock (StateT a m) where
  liftBlock = lift . liftBlock
instance (MonadBlock m) => MonadBlock (ReaderT a m) where
  liftBlock = lift . liftBlock
instance (MonadBlock m, Monoid a) => MonadBlock (WriterT a m) where
  liftBlock = lift . liftBlock

updateBlock :: MonadBlock m => BlockOutput -> m ()
updateBlock blockOutput = liftBlock . yield $ (Just (blockOutput, Nothing), DefaultUpdate)

updateBlock' :: MonadBlock m => BlockEventHandler -> BlockOutput -> m ()
updateBlock' blockEventHandler blockOutput = liftBlock . yield $ (Just (blockOutput, Just blockEventHandler), DefaultUpdate)

-- |Update a block by removing the current output
updateBlockEmpty :: MonadBlock m => m ()
updateBlockEmpty = liftBlock . yield $ (Nothing, DefaultUpdate)


mkBlockState :: BlockOutput -> BlockState
mkBlockState blockOutput = Just (blockOutput, Nothing)

mkBlockState' :: Text -> BlockEventHandler -> BlockOutput -> BlockState
mkBlockState' newBlockName blockEventHandler blockOutput = Just (blockOutput {_blockName = Just newBlockName}, Just blockEventHandler)

updateEventHandler :: BlockEventHandler -> BlockState -> BlockState
updateEventHandler _ Nothing = Nothing
updateEventHandler eventHandler (Just (blockOutput, _)) = Just (blockOutput, Just eventHandler)

hasEventHandler :: BlockState -> Bool
hasEventHandler (Just (_, Just _)) = True
hasEventHandler _ = False

invalidateBlockState :: BlockState -> BlockState
invalidateBlockState = (_Just . _1) %~ invalidateBlock


runBarIO :: MonadIO m => Bar -> BarIO r -> m r
runBarIO bar action = liftIO $ runReaderT (runSafeT action) bar


defaultInterval :: Interval
defaultInterval = everyNSeconds 10

-- |Converts a 'PullBlock' to a 'PushBlock' by running it whenever the 'defaultInterval' is triggered.
schedulePullBlock :: PullBlock -> PushBlock
schedulePullBlock = schedulePullBlock' defaultInterval

-- |Converts a 'PullBlock' to a 'PushBlock' by running it whenever the 'defaultInterval' is triggered.
schedulePullBlock' :: Interval -> PullBlock -> PushBlock
schedulePullBlock' interval pullBlock = PushMode <$ pullBlock >-> sleepToNextInterval
  where
    sleepToNextInterval :: Pipe BlockUpdate BlockUpdate BarIO PullMode
    sleepToNextInterval = do
      event <- liftIO Event.new
      forever $ do
        (state, _) <- await
        if hasEventHandler state
          then do
            -- If state already has an event handler, we do not attach another one
            yield (state, PullUpdate)
            sleepUntilInterval interval
          else do
            -- Attach a click handler that will trigger a block update
            yield (updateEventHandler (triggerOnClick event) state, PullUpdate)

            scheduler <- askSleepScheduler
            result <- liftIO $ do
              timerTask <- async $ sleepUntilInterval' scheduler defaultInterval
              eventTask <- async $ Event.wait event
              waitEitherCancel timerTask eventTask

            when (isRight result) $ do
              liftIO $ Event.clear event
              yield (invalidateBlockState state, UserUpdate)

    triggerOnClick :: Event -> BlockEvent -> BarIO ()
    triggerOnClick event _ = liftIO $ Event.set event

-- |Creates a new cache from a producer that automatically seals itself when the producer terminates.
newCache :: Producer [BlockState] BarIO () -> BlockCache
newCache input = newCacheInternal =<< newCache''
  where
    newCacheInternal :: (BlockCache, [BlockState] -> IO Bool, IO ()) -> BlockCache
    newCacheInternal (cache, update, seal) = do
      task <- barAsync updateTask
      liftIO $ link task
      cache
      where
        updateTask :: BarIO ()
        updateTask = do
          runEffect (input >-> forever (await >>= liftIO . update))
          liftIO seal

-- |Create a new cache. The result is a tuple of the cache, a consumer that can be used to update the cache and an action that seals the cache.
newCache' :: (MonadIO m, MonadIO m2, MonadIO m3) => m (BlockCache, Consumer [BlockState] m2 (), m3 ())
newCache' = do
  (cache, update, seal) <- newCache''
  return (cache, cacheUpdateConsumer update, seal)
  where
    cacheUpdateConsumer :: MonadIO m2 => ([BlockState] -> IO Bool) -> Consumer [BlockState] m2 ()
    cacheUpdateConsumer update = do
      v <- await
      result <- liftIO $ update v
      when result $ cacheUpdateConsumer update

-- |Low-level function to create a new cache. The result is a tuple of the cache, an action can be used to update the cache (it returns 'False'
-- |if the cache is sealed) and an action that seals the cache.
newCache'' :: (MonadIO m, MonadIO m2, MonadIO m3) => m (BlockCache, [BlockState] -> m2 Bool, m3 ())
newCache'' = do
  store <- liftIO $ newMVar (Just [])
  newCacheInternal store
  where
    newCacheInternal :: (MonadIO m, MonadIO m2, MonadIO m3) => MVar (Maybe [BlockState]) -> m (BlockCache, [BlockState] -> m2 Bool, m3 ())
    newCacheInternal store = return (cache, update, seal)
      where
        update :: MonadIO m => [BlockState] -> m Bool
        update value = liftIO $ modifyMVar store $ \old ->
          return $ case old of
            Nothing -> (Nothing, False)
            Just _ -> (Just value, True)
        seal :: MonadIO m => m ()
        seal = liftIO . void . swapMVar store $ Nothing
        cache :: BlockCache
        cache = do
          v <- liftIO (readMVar store)
          case v of
            Nothing -> exitCache
            Just value -> yield value >> cache

-- |Creates a cache from a push block.
cachePushBlock :: PushBlock -> BlockCache
cachePushBlock pushBlock = newCache $ () <$ (pushBlock >-> updateBarP >-> fixBlockName >-> PP.map (\a -> [a]))
  where
    updateBarP :: Pipe BlockUpdate BlockState BarIO r
    updateBarP = forever $ do
      (state, reason) <- await
      yield state
      updateBar reason

    -- |Sets 'blockName' to a random (but static) identifier if an event handler is set but the 'blockName' is not set.
    fixBlockName :: Pipe BlockState BlockState BarIO r
    fixBlockName = do
      defaultBlockName <- randomIdentifier
      forever $ do
        state <- await
        yield $ if hasEventHandler state
          then (_Just . _1 . blockName) %~ (Just . fromMaybe defaultBlockName) $ state
          else state



modify :: (BlockOutput -> BlockOutput) -> Pipe BlockUpdate BlockUpdate BarIO r
modify x = PP.map (over (_1 . _Just . _1) x)

autoPadding :: Pipe BlockUpdate BlockUpdate BarIO r
autoPadding = autoPadding' 0 0
  where
    autoPadding' :: Int64 -> Int64 -> Pipe BlockUpdate BlockUpdate BarIO r
    autoPadding' fullLength shortLength = do
      maybeBlock <- await
      case maybeBlock of
        (Just (block, eventHandler), reason) -> do
          let fullLength' = max fullLength . printedLength $ block^.fullText
          let shortLength' = max shortLength . printedLength $ block^.shortText._Just
          yield $ (Just (padFullText fullLength' . padShortText shortLength' $ block, eventHandler), reason)
          autoPadding' fullLength' shortLength'
        (Nothing, reason) -> do
          yield (Nothing, reason)
          autoPadding' 0 0
    padString :: Int64 -> BlockText
    padString len = normalText . T.take len . T.repeat $ ' '
    padFullText :: Int64 -> BlockOutput -> BlockOutput
    padFullText len = over fullText $ \s -> padString (len - printedLength s) <> s
    padShortText :: Int64 -> BlockOutput -> BlockOutput
    padShortText len = over (shortText._Just) $ \s -> padString (len - printedLength s) <> s


addBlock :: IsCachable a => a -> BarIO ()
addBlock block = do
  newBlockChan' <- newBlockChan <$> askBar
  liftIO $ atomically $ writeTChan newBlockChan' $ toCachedBlock block

updateBar :: MonadBarIO m => BlockUpdateReason -> m ()
updateBar reason = liftIO =<< requestBarUpdate <$> askBar <*> return reason

updateBar' :: MonadIO m => Bar -> BlockUpdateReason -> m ()
updateBar' bar reason = runBarIO bar $ updateBar reason

updateBarDefault :: MonadBarIO m => m ()
updateBarDefault = updateBar DefaultUpdate

updateBarDefault' :: MonadIO m => Bar -> m ()
updateBarDefault' bar = updateBar' bar DefaultUpdate

barAsync :: MonadBarIO m => BarIO a -> m (Async a)
barAsync action = do
  bar <- askBar
  liftIO $ async $ runBarIO bar action
