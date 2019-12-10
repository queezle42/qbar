{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module QBar.Core where

import QBar.BlockText

import Control.Exception (catch, finally, IOException)
import Control.Monad (forever)
import Control.Monad.Reader (ReaderT, runReaderT, ask, asks)
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
import qualified Pipes.Prelude as PP
import System.Exit
import System.IO
import System.Process.Typed (shell, withProcessWait, setStdin, setStdout, getStdout, closed, createPipe, readProcessStdout)
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

type Block a = Producer BlockOutput BarIO a


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


type BarIO = ReaderT Bar IO

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

modify :: (BlockOutput -> BlockOutput) -> Pipe BlockOutput BlockOutput BarIO r
modify = PP.map

autoPadding :: Pipe BlockOutput BlockOutput BarIO r
autoPadding = autoPadding' 0 0
  where
    autoPadding' :: Int64 -> Int64 -> Pipe BlockOutput BlockOutput BarIO r
    autoPadding' fullLength shortLength = do
      block <- await
      let fullLength' = max fullLength . printedLength $ block^.fullText
      let shortLength' = max shortLength . printedLength $ block^.shortText._Just -- TODO: ???
      yield $ padFullText fullLength' . padShortText shortLength' $ block
      autoPadding' fullLength' shortLength'
    padString :: Int64 -> BlockText
    padString len = normalText . T.take len . T.repeat $ ' '
    padFullText :: Int64 -> BlockOutput -> BlockOutput
    padFullText len = over fullText $ \s -> padString (len - printedLength s) <> s
    padShortText :: Int64 -> BlockOutput -> BlockOutput
    padShortText len = over (shortText._Just) $ \s -> padString (len - printedLength s) <> s

cacheFromInput :: Input BlockOutput -> CachedBlock
cacheFromInput input = CachedMode <$ fromInput input

-- | Create a shared interval. Takes a BarUpdateChannel to signal bar updates and an interval (in seconds).Data.Maybe
-- Returns an IO action that can be used to attach blocks to the shared interval and an async that contains a reference to the scheduler thread.
sharedInterval :: Int -> BarIO (PullBlock -> CachedBlock)
sharedInterval seconds = do
  clientsMVar <- liftIO $ newMVar ([] :: [(MVar PullBlock, Output BlockOutput)])

  startEvent <- liftIO Event.new

  task <- barAsync $ do
    -- Wait for at least one subscribed client
    liftIO $ Event.wait startEvent
    forever $ do
      liftIO $ threadDelay $ seconds * 1000000
      -- Updates all client blocks
      -- If send returns 'False' the clients mailbox has been closed, so it is removed
      bar <- ask
      liftIO $ modifyMVar_ clientsMVar $ fmap catMaybes . mapConcurrently (\r -> runReaderT (runAndFilterClient r) bar)
      -- Then update the bar
      updateBar

  liftIO $ link task

  return (addClient startEvent clientsMVar)
  where
    runAndFilterClient :: (MVar PullBlock, Output BlockOutput) -> BarIO (Maybe (MVar PullBlock, Output BlockOutput))
    runAndFilterClient client = do
      result <- runClient client
      return $ if result then Just client else Nothing
    runClient :: (MVar PullBlock, Output BlockOutput) -> BarIO Bool
    runClient (blockProducerMVar, output) = do
      bar <- ask
      liftIO $ modifyMVar blockProducerMVar $ \blockProducer -> do
        result <- runReaderT (next blockProducer) bar
        case result of
          Left _ -> return (exitBlock, False)
          Right (blockOutput, blockProducer') -> do
            success <- atomically $ send output blockOutput {
              _clickAction = Just (updateClickHandler blockOutput)
            }
            if success
              -- Store new BlockProducer back into MVar
              then return (blockProducer', True)
              -- Mailbox is sealed, stop running producer
              else return (exitBlock, False)
      where
        updateClickHandler :: BlockOutput -> Click -> BarIO ()
        updateClickHandler block _ = do
          -- Give user feedback that the block is updating
          let outdatedBlock = block & invalid.~True
          liftIO $ void $ atomically $ send output outdatedBlock
          -- Notify bar about changed block state to display the feedback
          updateBar
          -- Run a normal block update to update the block to the new value
          void $ runClient (blockProducerMVar, output)
          -- Notify bar about changed block state, this is usually done by the shared interval handler
          updateBar
    addClient :: Event.Event -> MVar [(MVar PullBlock, Output BlockOutput)] -> PullBlock -> CachedBlock
    addClient startEvent clientsMVar blockProducer = do
      -- Spawn the mailbox that preserves the latest block
      (output, input) <- liftIO $ spawn $ latest emptyBlock

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
blockScript path = forever $ yield =<< (lift blockScriptAction)
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

startPersistentBlockScript :: FilePath -> CachedBlock
-- This is only using 'CachedBlock' because the code was already written and tested
-- This could probably be massively simplified by using the new 'pushBlock'
startPersistentBlockScript path = do
  bar <- lift ask
  do
    (output, input, seal) <- liftIO $ spawn' $ latest emptyBlock
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
            runEffect $ yield (createErrorBlock $ "[" <> T.pack (show (e :: IOException)) <> "]") >-> signalFirstBlock initialDataEvent >-> toOutput output
          )
        )
        (atomically seal)
    liftIO $ link task
    liftIO $ Event.wait initialDataEvent
    cacheFromInput input
  where
    signalFirstBlock :: Event.Event -> Pipe BlockOutput BlockOutput IO ()
    signalFirstBlock event = do
      -- Await first block
      await >>= yield
      lift $ Event.set event
      -- Replace with cat
      cat
    fromHandle :: Bar -> Handle -> Producer BlockOutput IO ()
    fromHandle bar handle = forever $ do
      line <- lift $ TIO.hGetLine handle
      yield $ createBlock . pangoText $ line
      lift $ updateBar' bar


addBlock :: IsBlock a => a -> BarIO ()
addBlock block = do
  newBlockChan' <- asks newBlockChan
  liftIO $ atomically $ writeTChan newBlockChan' $ toCachedBlock block

updateBar :: BarIO ()
updateBar = liftIO =<< asks requestBarUpdate

updateBar' :: Bar -> IO ()
updateBar' = runReaderT updateBar

barAsync :: BarIO a -> BarIO (Async a)
barAsync action = do
  bar <- ask
  lift $ async $ runReaderT action bar

cachePushBlock :: PushBlock -> CachedBlock
cachePushBlock pushBlock = lift (next pushBlock) >>= either (const exitBlock) withInitialBlock
  where
    withInitialBlock :: (BlockOutput, PushBlock) -> CachedBlock
    withInitialBlock (initialBlockOutput, pushBlock') = do
      (output, input, seal) <- liftIO $ spawn' $ latest $ Just initialBlockOutput
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
    sendOutputToMailbox :: Output (Maybe BlockOutput) -> BlockOutput -> Effect BarIO ()
    sendOutputToMailbox output blockOutput = do
      -- The void is discarding the boolean result that indicates if the mailbox is sealed
      -- This is ok because a cached block is never sealed from the receiving side
      liftIO $ atomically $ void $ send output $ Just blockOutput
      lift updateBar
    terminateOnMaybe :: Producer (Maybe BlockOutput) BarIO () -> Producer BlockOutput BarIO CachedMode
    terminateOnMaybe p = do
      eitherMaybeValue <- lift $ next p
      case eitherMaybeValue of
        Right (Just value, newP) -> yield value >> terminateOnMaybe newP
        _ -> exitBlock
