{-# LANGUAGE TemplateHaskell #-}

module QBar.Core where

import QBar.Pango

import Control.Exception (catch, finally, IOException)
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.Event as Event
import Control.Concurrent.MVar
import Data.Aeson.TH
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.HashMap.Lazy as HM
import Data.Int (Int64)
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy.IO as TIO
import Numeric (showHex)
import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as PP
import System.Exit
import System.IO
import System.Process.Typed (shell, withProcessWait, setStdin, setStdout, getStdout, closed, createPipe, readProcessStdout)

import Data.Colour.RGBSpace

data BlockOutput = BlockOutput {
  values :: HM.HashMap T.Text T.Text,
  clickAction :: Maybe (Click -> IO ())
}
instance Show BlockOutput where
  show BlockOutput{values} = show values

data Click = Click {
  name :: T.Text,
  button :: Int
} deriving Show
$(deriveJSON defaultOptions ''Click)

type BlockProducer = Producer BlockOutput IO ()

-- |Block that 'yield's an update whenever the block should be changed
newtype PushBlockProducer = PushBlockProducer BlockProducer
-- |Block that generates an update on 'yield'. Should only be pulled when an update is required.
newtype PullBlockProducer = PullBlockProducer BlockProducer
-- |Cached block. Always 'yield's the latest update, so it should only be pulled when the bar is rendered.
newtype CachedBlockProducer = CachedBlockProducer BlockProducer

-- |Generic block type that can be a block in pull-, push- or cached mode.
data Block = PushBlock PushBlockProducer
  -- | PullBlock PullBlockProducer
  | CachedBlock CachedBlockProducer

data BarUpdateChannel = BarUpdateChannel (IO ())
type BarUpdateEvent = Event.Event

pushBlock :: BlockProducer -> Block
pushBlock = PushBlock . PushBlockProducer

--pullBlock :: BlockProducer -> Block
--pullBlock = PullBlock . PullBlockProducer

cachedBlock :: BlockProducer -> Block
cachedBlock = CachedBlock . CachedBlockProducer

pullBlockProducer :: BlockProducer -> PullBlockProducer
pullBlockProducer = PullBlockProducer


defaultColor :: T.Text
defaultColor = "#969896"

activeColor :: T.Text
activeColor = "#ffffff"

updatingColor :: T.Text
--updatingColor = "#444444"
updatingColor = "#96989677"

createBlock :: T.Text -> BlockOutput
createBlock text = setColor defaultColor $ BlockOutput {
  values = HM.singleton "full_text" text,
  clickAction = Nothing
}

createErrorBlock :: T.Text -> BlockOutput
createErrorBlock = setColor "ff0000" . createBlock

setValue :: T.Text -> T.Text -> BlockOutput -> BlockOutput
setValue key val block = block {
  values = HM.insert key val (values block)
}

getValue :: T.Text -> BlockOutput -> Maybe T.Text
getValue key block = HM.lookup key (values block)

adjustValue :: (T.Text -> T.Text) -> T.Text -> BlockOutput -> BlockOutput
adjustValue f k block = block {
  values = HM.adjust f k (values block)
}

emptyBlock :: BlockOutput
emptyBlock = createBlock ""

shortText :: T.Text -> BlockOutput -> BlockOutput
shortText = setValue "short_text"

fullText :: T.Text -> BlockOutput -> BlockOutput
fullText = setValue "full_text"

getFullText :: BlockOutput -> T.Text
getFullText = fromMaybe "" . getValue "full_text"

setColor :: T.Text -> BlockOutput -> BlockOutput
setColor = setValue "color"

setBlockName :: T.Text -> BlockOutput -> BlockOutput
setBlockName = setValue "name"

getBlockName :: BlockOutput -> Maybe T.Text
getBlockName = getValue "name"

pangoMarkup :: BlockOutput -> BlockOutput
pangoMarkup = setValue "markup" "pango"

adjustText :: (T.Text -> T.Text) -> BlockOutput -> BlockOutput
adjustText f = adjustValue f "full_text" . adjustValue f "short_text"

coloredText :: T.Text -> T.Text -> T.Text
coloredText color text = "<span color='" <> color <> "'>" <> text <> "</span>"

addIcon :: T.Text -> BlockOutput -> BlockOutput
addIcon icon block = prefixIcon "full_text" $ prefixIcon "short_text" block
  where
    prefixIcon = adjustValue ((icon <> " ") <>)

removePango :: BlockOutput -> BlockOutput
removePango block
  | getValue "markup" block == Just "pango" = adjustText removePangoFromText $ block {
      values = HM.delete "markup" (values block)
    }
  | otherwise = block
  where
    removePangoFromText :: T.Text -> T.Text
    removePangoFromText text =
      case parsePango text of
        Left _ -> text
        Right parsed -> removeFormatting parsed

modify :: (BlockOutput -> BlockOutput) -> Pipe BlockOutput BlockOutput IO ()
modify = PP.map

autoPadding :: Pipe BlockOutput BlockOutput IO ()
autoPadding = autoPadding' 0 0
  where
    autoPadding' :: Int64 -> Int64 -> Pipe BlockOutput BlockOutput IO ()
    autoPadding' fullLength shortLength = do
      block <- await
      let values' = (values block)
      let fullLength' = T.length $ HM.lookupDefault "" "full_text" values'
      let shortLength' = T.length $ HM.lookupDefault "" "short_text" values'
      let values'' = HM.adjust (<> (T.take (fullLength - fullLength') $ T.repeat ' ')) "full_text" values'
      let values''' = HM.adjust (<> (T.take (shortLength - shortLength') $ T.repeat ' ')) "short_text" values''
      yield block { values = values''' }
      autoPadding' (max fullLength fullLength') (max shortLength shortLength')

-- | Create a shared interval. Takes a BarUpdateChannel to signal bar updates and an interval (in seconds).Data.Maybe
-- Returns an IO action that can be used to attach blocks to the shared interval and an async that contains a reference to the scheduler thread.
sharedInterval :: BarUpdateChannel -> Int -> IO (PullBlockProducer -> Block, Async ())
sharedInterval barUpdateChannel seconds = do
  clientsMVar <- newMVar ([] :: [(MVar PullBlockProducer, Output BlockOutput)])

  task <- async $ forever $ do
    threadDelay $ seconds * 1000000
    -- Updates all client blocks
    -- If send returns 'False' the clients mailbox has been closed, so it is removed
    modifyMVar_ clientsMVar (fmap catMaybes . mapConcurrently runAndFilterClient)
    -- Then update the bar
    updateBar barUpdateChannel

  return (addClient clientsMVar, task)
    where
      runAndFilterClient :: (MVar PullBlockProducer, Output BlockOutput) -> IO (Maybe (MVar PullBlockProducer, Output BlockOutput))
      runAndFilterClient client = do
        result <- runClient client
        return $ if result then Just client else Nothing
      runClient :: (MVar PullBlockProducer, Output BlockOutput) -> IO Bool
      runClient (blockProducerMVar, output) =
        modifyMVar blockProducerMVar $ \(PullBlockProducer blockProducer) -> do
          result <- next blockProducer
          case result of
            Left () -> return (PullBlockProducer $ return (), False)
            Right (blockOutput, blockProducer') -> do
              success <- atomically $ send output blockOutput {
                clickAction = Just (updateClickHandler blockOutput)
              }
              if success
                -- Store new BlockProducer back into MVar
                then return (pullBlockProducer blockProducer', True)
                -- Mailbox is sealed, stop running producer
                else return (PullBlockProducer $ return (), False)
        where
          updateClickHandler :: BlockOutput -> Click -> IO ()
          updateClickHandler block _ = do
            -- Give user feedback that the block is updating
            let outdatedBlock = setColor updatingColor $ removePango block
            void $ atomically $ send output $ outdatedBlock
            -- Notify bar about changed block state to display the feedback
            updateBar barUpdateChannel
            -- Run a normal block update to update the block to the new value
            void $ runClient (blockProducerMVar, output)
            -- Notify bar about changed block state, this is usually done by the shared interval handler
            updateBar barUpdateChannel
      addClient :: MVar [(MVar PullBlockProducer, Output BlockOutput)] -> PullBlockProducer -> Block
      addClient clientsMVar blockProducer = cachedBlock $ do
        -- Spawn the mailbox that preserves the latest block
        (output, input) <- lift $ spawn $ latest emptyBlock

        blockProducerMVar <- lift $ newMVar blockProducer

        -- Generate initial block and send it to the mailbox
        lift $ void $ runClient (blockProducerMVar, output)

        -- Register the client for regular updates
        lift $ modifyMVar_ clientsMVar $ \ clients -> return ((blockProducerMVar, output):clients)

        -- Return a block producer from the mailbox
        fromInput input

blockScript :: FilePath -> PullBlockProducer
blockScript path = pullBlockProducer $ forever $ yield =<< (lift $ blockScriptAction)
  where
    blockScriptAction :: IO BlockOutput
    blockScriptAction = do
      -- The exit code is used for i3blocks signaling but ignored here (=not implemented)
      -- I am trying to replace i3blocks scripts with native haskell blocks, so I do not need it
      (exitCode, output) <- readProcessStdout $ shell path
      case exitCode of
        ExitSuccess -> return $ case map E.decodeUtf8 (C8.lines output) of
          (text:short:color:_) -> setColor color $ shortText short $ createScriptBlock text
          (text:short:_) -> shortText short $ createScriptBlock text
          (text:_) -> createScriptBlock text
          [] -> createScriptBlock "-"
        (ExitFailure nr) -> return $ createErrorBlock $ "[" <> (T.pack $ show nr) <> "]"
    createScriptBlock :: T.Text -> BlockOutput
    createScriptBlock text = pangoMarkup $ setBlockName (T.pack path) $ createBlock text

startPersistentBlockScript :: BarUpdateChannel -> FilePath -> Block
-- This is only using 'cachedBlock' because the code was already written and tested
-- This could probably be massively simplified by using the new 'pushBlock'
startPersistentBlockScript barUpdateChannel path = cachedBlock $ do
  (output, input, seal) <- lift $ spawn' $ latest $ emptyBlock
  initialDataEvent <- lift $ Event.new
  task <- lift $ async $ do
    let processConfig = setStdin closed $ setStdout createPipe $ shell path
    finally (
      catch (
        withProcessWait processConfig $ \ process -> do
          let handle = getStdout process
          runEffect $ (fromHandle handle) >-> signalFirstBlock initialDataEvent >-> toOutput output
        )
        ( \ e ->
          -- output error
          runEffect $ (yield $ createErrorBlock $ "[" <> (T.pack $ show (e :: IOException)) <> "]") >-> signalFirstBlock initialDataEvent >-> toOutput output
        )
      )
      (atomically seal)
  lift $ link task
  lift $ Event.wait initialDataEvent
  fromInput input
  where
    signalFirstBlock :: Event.Event -> Pipe BlockOutput BlockOutput IO ()
    signalFirstBlock event = do
      -- Await first block
      await >>= yield
      lift $ Event.set event
      -- Replace with cat
      cat
    fromHandle :: Handle -> Producer BlockOutput IO ()
    fromHandle handle = forever $ do
      line <- lift $ TIO.hGetLine handle
      yield $ pangoMarkup $ createBlock line
      lift $ updateBar barUpdateChannel

pangoColor :: RGB Double -> T.Text
pangoColor (RGB r g b) =
  let r' = hexColorComponent r
      g' = hexColorComponent g
      b' = hexColorComponent b
  in "#" <> r' <> g' <> b'
  where
    hexColorComponent :: Double -> T.Text
    hexColorComponent val = paddedHexComponent $ T.pack $ showHex (max 0 $ min 255 $ (truncate (val * 255) :: Int)) ""
    paddedHexComponent hex =
      let len = 2 - T.length hex
          padding = if len == 1 then "0" else ""
      in padding <> hex

updateBar :: BarUpdateChannel -> IO ()
updateBar (BarUpdateChannel updateAction) = updateAction

cachePushBlock :: BarUpdateChannel -> PushBlockProducer -> CachedBlockProducer
cachePushBlock barUpdateChannel (PushBlockProducer blockProducer) = CachedBlockProducer $
  lift (next blockProducer) >>= either (lift . return) withInitialBlock
  where
    withInitialBlock :: (BlockOutput, BlockProducer) -> BlockProducer
    withInitialBlock (initialBlockOutput, blockProducer') = do
      (output, input, seal) <- lift $ spawn' $ latest $ Just initialBlockOutput
      -- The async could be used to stop the block later, but for now we are just linking it to catch exceptions
      lift $ link =<< async (sendProducerToMailbox output seal blockProducer')
      terminateOnMaybe $ fromInput input
    sendProducerToMailbox :: Output (Maybe BlockOutput) -> STM () -> BlockProducer -> IO ()
    sendProducerToMailbox output seal blockProducer' = do
      runEffect $ for blockProducer' (sendOutputToMailbox output)
      atomically $ void $ send output Nothing
      updateBar barUpdateChannel
      atomically seal
    sendOutputToMailbox :: Output (Maybe BlockOutput) -> BlockOutput -> Effect IO ()
    sendOutputToMailbox output blockOutput = lift $ do
      -- The void is discarding the boolean result that indicates if the mailbox is sealed
      -- This is ok because a cached block is never sealed from the receiving side
      atomically $ void $ send output $ Just blockOutput
      updateBar barUpdateChannel
    terminateOnMaybe :: Producer (Maybe a) IO () -> Producer a IO ()
    terminateOnMaybe p = do
      eitherMaybeValue <- lift $ next p
      case eitherMaybeValue of
        Right (Just value, newP) -> yield value >> terminateOnMaybe newP
        _ -> return ()


blockToCachedBlockProducer :: BarUpdateChannel -> Block -> CachedBlockProducer
blockToCachedBlockProducer barUpdateChannel (PushBlock pushBlockProducer) = cachePushBlock barUpdateChannel pushBlockProducer
blockToCachedBlockProducer _ (CachedBlock cachedBlockProducer) = cachedBlockProducer

-- |The '>!>'-operator can be used to apply a 'Pipe' to the 'BlockProducer' contained in the 'Block'.
(>!>) :: Block -> Pipe BlockOutput BlockOutput IO () -> Block
(>!>) (PushBlock (PushBlockProducer blockProducer)) pipe = pushBlock $ (blockProducer >-> pipe)
(>!>) (CachedBlock (CachedBlockProducer blockProducer)) pipe = cachedBlock $ (blockProducer >-> pipe)
