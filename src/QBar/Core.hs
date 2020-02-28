{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module QBar.Core where

import QBar.BlockOutput
import QBar.Time

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
  toCachedBlock = toCachedBlock . schedulePullBlock
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
invalidateBlockState Nothing = Nothing
invalidateBlockState (Just (output, eventHandler)) = Just (invalidateBlock output, eventHandler)


runBarIO :: MonadIO m => Bar -> BarIO r -> m r
runBarIO bar action = liftIO $ runReaderT (runSafeT action) bar


defaultInterval :: Interval
defaultInterval = everyNSeconds 10

schedulePullBlock :: PullBlock -> PushBlock
schedulePullBlock pullBlock = PushMode <$ pullBlock >-> sleepToNextInterval
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
            sleepUntilInterval defaultInterval
          else do
            -- Attach a click handler that will trigger a block update
            yield $ (updateEventHandler (triggerOnClick event) state, PullUpdate)

            scheduler <- askSleepScheduler
            result <- liftIO $ do
              timerTask <- async $ sleepUntilInterval' scheduler defaultInterval
              eventTask <- async $ Event.wait event
              waitEitherCancel timerTask eventTask

            when (isRight result) $ yield $ (invalidateBlockState state, UserUpdate)

    triggerOnClick :: Event -> BlockEvent -> BarIO ()
    triggerOnClick event _ = liftIO $ Event.signal event

newCache :: Producer [BlockState] IO () -> BlockCache
newCache input = newCacheInternal =<< newCache''
  where
    newCacheInternal :: (BlockCache, [BlockState] -> IO Bool, IO ()) -> BlockCache
    newCacheInternal (cache, update, seal) = do
      liftIO $ link =<< async updateTask
      cache
      where
        updateTask :: IO ()
        updateTask = do
          runEffect (input >-> forever (await >>= liftIO . update))
          seal

newCache' :: (MonadIO m) => m (BlockCache, Consumer [BlockState] IO (), IO ())
newCache' = do
  (cache, update, seal) <- newCache''
  return (cache, cacheUpdateConsumer update, seal)
  where
    cacheUpdateConsumer :: ([BlockState] -> IO Bool) -> Consumer [BlockState] IO ()
    cacheUpdateConsumer update = do
      v <- await
      result <- liftIO $ update v
      when result $ cacheUpdateConsumer update

newCache'' :: (MonadIO m) => m (BlockCache, [BlockState] -> IO Bool, IO ())
newCache'' = do
  store <- liftIO $ newMVar (Just [])
  newCacheInternal store
  where
    newCacheInternal :: MonadIO m => MVar (Maybe [BlockState]) -> m (BlockCache, [BlockState] -> IO Bool, IO ())
    newCacheInternal store = return (cache, update, seal)
      where
        update :: [BlockState] -> IO Bool
        update value = modifyMVar store $ \old ->
          return $ case old of
            Nothing -> (Nothing, False)
            Just _ -> (Just value, True)
        seal :: IO ()
        seal = void . swapMVar store $ Nothing
        cache :: BlockCache
        cache = do
          v <- liftIO (readMVar store)
          case v of
            Nothing -> exitCache
            Just value -> yield value >> cache


cacheFromInput :: Input BlockState -> BlockCache
cacheFromInput input = do
  result <- liftIO $ atomically $ recv input
  case result of
    Nothing -> exitCache
    Just value -> yield [value] >> cacheFromInput input


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

barAsync :: BarIO a -> BarIO (Async a)
barAsync action = do
  bar <- askBar
  liftIO $ async $ runBarIO bar action

cachePushBlock :: PushBlock -> BlockCache
cachePushBlock pushBlock = lift (next pushBlock) >>= either (const exitCache) withInitialBlock
  where
    withInitialBlock :: (BlockUpdate, PushBlock) -> BlockCache
    withInitialBlock (initialBlockUpdate, pushBlock') = do
      let (initialBlockState, _) = initialBlockUpdate
      (output, input, seal) <- liftIO $ spawn' $ latest initialBlockState
      -- The async could be used to stop the block later, but for now we are just linking it to catch exceptions
      task <- lift $ barAsync (sendProducerToMailbox output seal pushBlock')
      liftIO $ link task
      cacheFromInput input
    sendProducerToMailbox :: Output BlockState -> STM () -> PushBlock -> BarIO ()
    sendProducerToMailbox output seal pushBlock' = do
      -- Send push block output to mailbox until it terminates
      void $ runEffect $ for pushBlock' (sendOutputToMailbox output)
      -- Then clear the block and seal the mailbox
      liftIO $ atomically $ void $ send output Nothing
      -- TODO: pass reason from BlockUpdate
      updateBarDefault
      -- TODO: sealing does prevent a 'latest' mailbox from being read
      liftIO $ atomically seal
    sendOutputToMailbox :: Output BlockState -> BlockUpdate -> Effect BarIO ()
    sendOutputToMailbox output blockUpdate = do
      let (state, _reason) = blockUpdate
      -- The void is discarding the boolean result that indicates if the mailbox is sealed
      -- This is ok because a cached block is never sealed from the receiving side
      liftIO $ atomically $ void $ send output state
      -- TODO signal update reason to renderer
      lift updateBarDefault
