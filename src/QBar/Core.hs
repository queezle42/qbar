{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module QBar.Core where

import QBar.BlockText

import Control.Exception (catch, finally, IOException)
import Control.Monad (forever)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.Event as Event
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan (TChan, writeTChan)
import Data.Aeson.TH
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Int (Int64)
import Data.Maybe (catMaybes)
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
  getStdout, closed, createPipe, withProcessWait, readProcessStdout, startProcess, stopProcess)
import Control.Lens


data Click = Click {
  name :: T.Text,
  button :: Int
} deriving Show
$(deriveJSON defaultOptions ''Click)

data BlockOutput = BlockOutput
  { _fullText :: BlockText
  , _shortText :: Maybe BlockText
  , _blockName :: Maybe T.Text
  , _clickAction :: Maybe (Click -> BarIO ())
  , _invalid :: Bool
  }


data PushMode = PushMode
data PullMode = PullMode
data CachedMode = CachedMode

type Block a = Producer (Maybe BlockOutput) BarIO a


-- |Block that 'yield's an update whenever the block should be changed
type PushBlock = Block PushMode
-- |Block that generates an update on 'yield'. Should only be pulled when an update is required.
type PullBlock = Block PullMode
-- |Cached block. Always 'yield's the latest update, so it should only be pulled when the bar is rendered.
type CachedBlock = Block CachedMode

class IsBlock a where
  toCachedBlock :: a -> CachedBlock

class IsBlockMode a where
  exitBlock :: Block a
instance IsBlockMode PushMode where
  exitBlock = return PushMode
instance IsBlockMode PullMode where
  exitBlock = return PullMode
instance IsBlockMode CachedMode where
  exitBlock = return CachedMode


type BarIO = SafeT (ReaderT Bar IO)

data Bar = Bar {
  requestBarUpdate :: IO (),
  newBlockChan :: TChan CachedBlock
}
makeLenses ''BlockOutput

instance IsBlock PushBlock where
  toCachedBlock = cachePushBlock
instance IsBlock CachedBlock where
  toCachedBlock = id

data BarUpdateChannel = BarUpdateChannel (IO ())
type BarUpdateEvent = Event.Event


runBarIO :: Bar -> BarIO r -> IO r
runBarIO bar action = runReaderT (runSafeT action) bar

askBar :: BarIO Bar
askBar = lift ask

createBlock :: BlockText -> BlockOutput
createBlock text = BlockOutput
  { _fullText = text
  , _shortText = Nothing
  , _blockName = Nothing
  , _clickAction = Nothing
  , _invalid = False
  }

createErrorBlock :: T.Text -> BlockOutput
createErrorBlock = createBlock . importantText criticalImportant

emptyBlock :: BlockOutput
emptyBlock = createBlock mempty

addIcon :: T.Text -> BlockOutput -> BlockOutput
addIcon icon = over fullText $ (<>) . normalText $ icon <> " "

modify :: (BlockOutput -> BlockOutput)
       -> Pipe (Maybe BlockOutput) (Maybe BlockOutput) BarIO r
modify x = PP.map (x <$>)

autoPadding :: Pipe (Maybe BlockOutput) (Maybe BlockOutput) BarIO r
autoPadding = autoPadding' 0 0
  where
    autoPadding' :: Int64 -> Int64 -> Pipe (Maybe BlockOutput) (Maybe BlockOutput) BarIO r
    autoPadding' fullLength shortLength = do
      maybeBlock <- await
      case maybeBlock of
        Just block -> do
          let fullLength' = max fullLength . printedLength $ block^.fullText
          let shortLength' = max shortLength . printedLength $ block^.shortText._Just -- TODO: ???
          yield $ Just $ padFullText fullLength' . padShortText shortLength' $ block
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

cacheFromInput :: Input (Maybe BlockOutput) -> CachedBlock
cacheFromInput input = CachedMode <$ fromInput input

-- | Create a shared interval. Takes a BarUpdateChannel to signal bar updates and an interval (in seconds).Data.Maybe
-- Returns an IO action that can be used to attach blocks to the shared interval and an async that contains a reference to the scheduler thread.
sharedInterval :: Int -> BarIO (PullBlock -> CachedBlock)
sharedInterval seconds = do
  clientsMVar <- liftIO $ newMVar ([] :: [(MVar PullBlock, Output (Maybe BlockOutput))])

  startEvent <- liftIO Event.new

  task <- barAsync $ do
    -- Wait for at least one subscribed client
    liftIO $ Event.wait startEvent
    forever $ do
      liftIO $ threadDelay $ seconds * 1000000
      -- Updates all client blocks
      -- If send returns 'False' the clients mailbox has been closed, so it is removed
      bar <- askBar
      liftIO $ modifyMVar_ clientsMVar $ fmap catMaybes . mapConcurrently (\r -> runBarIO bar $ runAndFilterClient r)
      -- Then update the bar
      updateBar

  liftIO $ link task

  return (addClient startEvent clientsMVar)
  where
    runAndFilterClient :: (MVar PullBlock, Output (Maybe BlockOutput)) -> BarIO (Maybe (MVar PullBlock, Output (Maybe BlockOutput)))
    runAndFilterClient client = do
      result <- runClient client
      return $ if result then Just client else Nothing
    runClient :: (MVar PullBlock, Output (Maybe BlockOutput)) -> BarIO Bool
    runClient (blockProducerMVar, output) = do
      bar <- askBar
      liftIO $ modifyMVar blockProducerMVar $ \blockProducer -> do
        result <- runReaderT (runSafeT $ next blockProducer) bar
        case result of
          Left _ -> return (exitBlock, False)
          Right (blockOutput, blockProducer') -> do
            success <- atomically $ send output $ (clickAction ?~ updateClickHandler blockOutput) <$> blockOutput
            if success
              -- Store new BlockProducer back into MVar
              then return (blockProducer', True)
              -- Mailbox is sealed, stop running producer
              else return (exitBlock, False)
      where
        updateClickHandler :: Maybe BlockOutput -> Click -> BarIO ()
        updateClickHandler Nothing _ = return ()
        updateClickHandler (Just block) _ = do
          -- Give user feedback that the block is updating
          let outdatedBlock = block & invalid.~True
          liftIO $ void $ atomically $ send output . Just $ outdatedBlock
          -- Notify bar about changed block state to display the feedback
          updateBar
          -- Run a normal block update to update the block to the new value
          void $ runClient (blockProducerMVar, output)
          -- Notify bar about changed block state, this is usually done by the shared interval handler
          updateBar
    addClient :: Event.Event -> MVar [(MVar PullBlock, Output (Maybe BlockOutput))] -> PullBlock -> CachedBlock
    addClient startEvent clientsMVar blockProducer = do
      -- Spawn the mailbox that preserves the latest block
      (output, input) <- liftIO $ spawn $ latest $ Just emptyBlock

      blockProducerMVar <- liftIO $ newMVar blockProducer

      -- Generate initial block and send it to the mailbox
      lift $ void $ runClient (blockProducerMVar, output)

      -- Register the client for regular updates
      liftIO $ modifyMVar_ clientsMVar $ \ clients -> return ((blockProducerMVar, output):clients)

      -- Start update thread (if not already started)
      liftIO $ Event.set startEvent

      -- Return a block producer from the mailbox
      cacheFromInput input

blockScript :: FilePath -> PullBlock
blockScript path = forever $ yield . Just =<< (lift blockScriptAction)
  where
    blockScriptAction :: BarIO BlockOutput
    blockScriptAction = do
      -- The exit code is used for i3blocks signaling but ignored here (=not implemented)
      -- I am trying to replace i3blocks scripts with native haskell blocks, so I do not need it
      (exitCode, output) <- liftIO $ readProcessStdout $ shell path
      case exitCode of
        ExitSuccess -> return $ case map E.decodeUtf8 (C8.lines output) of
          -- TODO: Fix this, but how?
          --   PangoSegments cannot have external formatting, so either allow that here,
          --   or duplicate the function into ango and nonPango variants.
          -- (text:short:color:_) -> setColor color $ shortText short $ createScriptBlock text
          (text:short:_) -> shortText ?~ pangoText short $ createScriptBlock text
          (text:_) -> createScriptBlock text
          [] -> createScriptBlock "-"
        (ExitFailure nr) -> return $ createErrorBlock $ "[" <> T.pack (show nr) <> "]"
    createScriptBlock :: T.Text -> BlockOutput
    createScriptBlock text = blockName ?~ T.pack path $ createBlock . pangoText $ text

startPersistentBlockScript' :: FilePath -> CachedBlock
-- This is only using 'CachedBlock' because the code was already written and tested
-- This could probably be massively simplified by using the new 'pushBlock'
startPersistentBlockScript' path = do
  bar <- lift askBar
  do
    (output, input, seal) <- liftIO $ spawn' $ latest $ Nothing
    initialDataEvent <- liftIO Event.new
    task <- liftIO $ async $ do
      let processConfig = setStdin closed $ setStdout createPipe $ shell path
      finally (
        catch (
          withProcessWait processConfig $ \ process -> do
            let handle = getStdout process
            runEffect $ fromHandle bar handle >-> signalFirstBlock initialDataEvent >-> toOutput output
          )
          ( \ e ->
            -- output error
            runEffect $ yield (Just . createErrorBlock $ "[" <> T.pack (show (e :: IOException)) <> "]") >-> signalFirstBlock initialDataEvent >-> toOutput output
          )
        )
        (atomically seal)
    liftIO $ link task
    liftIO $ Event.wait initialDataEvent
    cacheFromInput input
  where
    signalFirstBlock :: Event.Event -> Pipe (Maybe BlockOutput) (Maybe BlockOutput) IO ()
    signalFirstBlock event = do
      -- Await first block
      await >>= yield
      lift $ Event.set event
      -- Replace with cat
      cat
    fromHandle :: Bar -> Handle -> Producer (Maybe BlockOutput) IO ()
    fromHandle bar handle = forever $ do
      line <- lift $ TIO.hGetLine handle
      yield $ Just . createBlock . pangoText $ line
      lift $ updateBar' bar

startPersistentBlockScript :: FilePath -> PushBlock
-- The outer catchP only catches errors that occur during process creation
startPersistentBlockScript path = catchP startScriptProcess handleError
  where
    handleError :: IOException -> PushBlock
    handleError e = do
      yield . Just . createErrorBlock $ "[" <> T.pack (show e) <> "]"
      return PushMode
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
      yield $ Just . createBlock . pangoText $ line
      lift updateBar

addBlock :: IsBlock a => a -> BarIO ()
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

cachePushBlock :: PushBlock -> CachedBlock
cachePushBlock pushBlock = lift (next pushBlock) >>= either (const exitBlock) withInitialBlock
  where
    withInitialBlock :: (Maybe BlockOutput, PushBlock) -> CachedBlock
    withInitialBlock (initialBlockOutput, pushBlock') = do
      (output, input, seal) <- liftIO $ spawn' $ latest $ initialBlockOutput
      -- The async could be used to stop the block later, but for now we are just linking it to catch exceptions
      task <- lift $ barAsync (sendProducerToMailbox output seal pushBlock')
      liftIO $ link task
      terminateOnMaybe $ fromInput input
    sendProducerToMailbox :: Output (Maybe BlockOutput) -> STM () -> PushBlock -> BarIO ()
    sendProducerToMailbox output seal pushBlock' = do
      void $ runEffect $ for pushBlock' (sendOutputToMailbox output)
      liftIO $ atomically $ void $ send output Nothing
      updateBar
      liftIO $ atomically seal
    sendOutputToMailbox :: Output (Maybe BlockOutput) -> Maybe BlockOutput -> Effect BarIO ()
    sendOutputToMailbox output blockOutput = do
      -- The void is discarding the boolean result that indicates if the mailbox is sealed
      -- This is ok because a cached block is never sealed from the receiving side
      liftIO $ atomically $ void $ send output $ blockOutput
      lift updateBar
    terminateOnMaybe :: Producer (Maybe BlockOutput) BarIO () -> Producer (Maybe BlockOutput) BarIO CachedMode
    terminateOnMaybe p = do
      eitherMaybeValue <- lift $ next p
      case eitherMaybeValue of
        Right (Just value, newP) -> yield (Just value) >> terminateOnMaybe newP
        _ -> exitBlock
