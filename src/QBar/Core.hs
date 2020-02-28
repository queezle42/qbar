{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module QBar.Core where

import QBar.BlockOutput
import QBar.TagParser
import QBar.Time

import Control.Concurrent.Async
import Control.Concurrent.Event as Event
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan (TChan, writeTChan)
import Control.Exception (IOException)
import Control.Lens
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT)
import Data.Aeson.TH
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Either (isRight)
import Data.Int (Int64)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy.IO as TIO
import Pipes
import Pipes.Concurrent
import Pipes.Safe (SafeT, catchP, runSafeT)
import qualified Pipes.Prelude as PP
import System.Exit
import System.IO
import System.Process.Typed (Process, shell, setStdin, setStdout,
  getStdout, closed, createPipe, readProcessStdout, startProcess, stopProcess)


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

type Block = Producer BlockState BarIO


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
  requestBarUpdate :: IO (),
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
updateBlock blockOutput = liftBlock . yield $ Just (blockOutput, Nothing)

updateBlock' :: MonadBlock m => BlockEventHandler -> BlockOutput -> m ()
updateBlock' blockEventHandler blockOutput = liftBlock . yield $ Just (blockOutput, Just blockEventHandler)

-- |Update a block by removing the current output
updateBlockEmpty :: MonadBlock m => m ()
updateBlockEmpty = liftBlock . yield $ Nothing


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


runBarIO :: Bar -> BarIO r -> IO r
runBarIO bar action = runReaderT (runSafeT action) bar


defaultInterval :: Interval
defaultInterval = everyNSeconds 10

schedulePullBlock :: PullBlock -> PushBlock
schedulePullBlock pullBlock = PushMode <$ pullBlock >-> sleepToNextInterval
  where
    sleepToNextInterval :: Pipe BlockState BlockState BarIO PullMode
    sleepToNextInterval = do
      event <- liftIO Event.new
      forever $ do
        state <- await
        if hasEventHandler state
          then do
            -- If state already has an event handler, we do not attach another one
            yield state
            sleepUntilInterval defaultInterval
          else do
            -- Attach a click handler that will trigger a block update
            yield $ updateEventHandler (triggerOnClick event) state

            scheduler <- askSleepScheduler
            result <- liftIO $ do
              timerTask <- async $ sleepUntilInterval' scheduler defaultInterval
              eventTask <- async $ Event.wait event
              waitEitherCancel timerTask eventTask

            when (isRight result) $ yield $ invalidateBlockState state

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


modify :: (BlockOutput -> BlockOutput) -> Pipe BlockState BlockState BarIO r
modify x = PP.map (over (_Just . _1) x)

autoPadding :: Pipe BlockState BlockState BarIO r
autoPadding = autoPadding' 0 0
  where
    autoPadding' :: Int64 -> Int64 -> Pipe BlockState BlockState BarIO r
    autoPadding' fullLength shortLength = do
      maybeBlock <- await
      case maybeBlock of
        Just (block, eventHandler) -> do
          let fullLength' = max fullLength . printedLength $ block^.fullText
          let shortLength' = max shortLength . printedLength $ block^.shortText._Just
          yield $ Just (padFullText fullLength' . padShortText shortLength' $ block, eventHandler)
          autoPadding' fullLength' shortLength'
        Nothing -> do
          yield Nothing
          autoPadding' 0 0
    padString :: Int64 -> BlockText
    padString len = normalText . T.take len . T.repeat $ ' '
    padFullText :: Int64 -> BlockOutput -> BlockOutput
    padFullText len = over fullText $ \s -> padString (len - printedLength s) <> s
    padShortText :: Int64 -> BlockOutput -> BlockOutput
    padShortText len = over (shortText._Just) $ \s -> padString (len - printedLength s) <> s


blockScript :: FilePath -> PullBlock
blockScript path = forever $ updateBlock =<< (lift blockScriptAction)
  where
    blockScriptAction :: BarIO BlockOutput
    blockScriptAction = do
      -- The exit code is used for i3blocks signaling but ignored here (=not implemented)
      -- I am trying to replace i3blocks scripts with native haskell blocks, so I do not need it
      (exitCode, output) <- liftIO $ readProcessStdout $ shell path
      return $ case exitCode of
        ExitSuccess -> createScriptBlock output
        (ExitFailure nr) -> case nr of
          _ -> mkErrorOutput $ "exit code " <> T.pack (show nr) <> ""
    createScriptBlock :: C8.ByteString -> BlockOutput
    createScriptBlock output = case map E.decodeUtf8 (C8.lines output) of
      (text:short:_) -> parseTags'' text short
      (text:_) -> parseTags' text
      [] -> emptyBlock

persistentBlockScript :: FilePath -> PushBlock
-- The outer catchP only catches errors that occur during process creation
persistentBlockScript path = catchP startScriptProcess handleError
  where
    handleError :: IOException -> PushBlock
    handleError e = do
      updateBlock . mkErrorOutput $ T.pack (show e)
      exitBlock
    handleErrorWithProcess :: (Process i o e) -> IOException -> PushBlock
    handleErrorWithProcess process e = do
      stopProcess process
      handleError e
    startScriptProcess :: PushBlock
    startScriptProcess = do
      let processConfig = setStdin closed $ setStdout createPipe $ shell path
      process <- startProcess processConfig
      -- The inner catchP catches errors that happen after the process has been created
      -- This handler will also make sure the process is stopped
      catchP (blockFromHandle $ getStdout process) (handleErrorWithProcess process)
    blockFromHandle :: Handle -> PushBlock
    blockFromHandle handle = forever $ do
      line <- liftIO $ TIO.hGetLine handle
      updateBlock $ parseTags' line
      lift updateBar

addBlock :: IsCachable a => a -> BarIO ()
addBlock block = do
  newBlockChan' <- newBlockChan <$> askBar
  liftIO $ atomically $ writeTChan newBlockChan' $ toCachedBlock block

updateBar :: BarIO ()
updateBar = liftIO =<< requestBarUpdate <$> askBar

updateBar' :: Bar -> IO ()
updateBar' bar = runBarIO bar updateBar

barAsync :: BarIO a -> BarIO (Async a)
barAsync action = do
  bar <- askBar
  liftIO $ async $ runBarIO bar action

cachePushBlock :: PushBlock -> BlockCache
cachePushBlock pushBlock = lift (next pushBlock) >>= either (const exitCache) withInitialBlock
  where
    withInitialBlock :: (BlockState, PushBlock) -> BlockCache
    withInitialBlock (initialBlockOutput, pushBlock') = do
      (output, input, seal) <- liftIO $ spawn' $ latest initialBlockOutput
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
      updateBar
      -- TODO: sealing does prevent a 'latest' mailbox from being read
      liftIO $ atomically seal
    sendOutputToMailbox :: Output BlockState -> BlockState -> Effect BarIO ()
    sendOutputToMailbox output blockOutput = do
      -- The void is discarding the boolean result that indicates if the mailbox is sealed
      -- This is ok because a cached block is never sealed from the receiving side
      liftIO $ atomically $ void $ send output blockOutput
      lift updateBar
