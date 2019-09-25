{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

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

data Block = Block {
  values :: HM.HashMap T.Text T.Text,
  clickAction :: Maybe (IO ())
}
instance Show Block where
  show Block{values} = show values

data Click = Click {
  name :: T.Text
} deriving Show
$(deriveJSON defaultOptions ''Click)

type BlockProducer = Producer Block IO ()

data BarUpdateChannel = BarUpdateChannel (IO ())
type BarUpdateEvent = Event.Event

defaultColor :: T.Text
defaultColor = "#969896"

activeColor :: T.Text
activeColor = "#ffffff"

updatingColor :: T.Text
--updatingColor = "#444444"
updatingColor = "#96989677"

createBlock :: T.Text -> Block
createBlock text = setColor defaultColor $ Block {
  values = HM.singleton "full_text" text,
  clickAction = Nothing
}

createErrorBlock :: T.Text -> Block
createErrorBlock = setColor "ff0000" . createBlock

setValue :: T.Text -> T.Text -> Block -> Block
setValue key val block = block {
  values = HM.insert key val (values block)
}

getValue :: T.Text -> Block -> Maybe T.Text
getValue key block = HM.lookup key (values block)

adjustValue :: (T.Text -> T.Text) -> T.Text -> Block -> Block
adjustValue f k block = block {
  values = HM.adjust f k (values block)
}

emptyBlock :: Block
emptyBlock = createBlock ""

shortText :: T.Text -> Block -> Block
shortText = setValue "short_text"

fullText :: T.Text -> Block -> Block
fullText = setValue "full_text"

getFullText :: Block -> T.Text
getFullText = fromMaybe "" . getValue "full_text"

setColor :: T.Text -> Block -> Block
setColor = setValue "color"

setBlockName :: T.Text -> Block -> Block
setBlockName = setValue "name"

getBlockName :: Block -> Maybe T.Text
getBlockName = getValue "name"

pangoMarkup :: Block -> Block
pangoMarkup = setValue "markup" "pango"

adjustText :: (T.Text -> T.Text) -> Block -> Block
adjustText f = adjustValue f "full_text" . adjustValue f "short_text"

coloredText :: T.Text -> T.Text -> T.Text
coloredText color text = "<span color='" <> color <> "'>" <> text <> "</span>"

addIcon :: T.Text -> Block -> Block
addIcon icon block = prefixIcon "full_text" $ prefixIcon "short_text" block
  where
    prefixIcon = adjustValue ((icon <> " ") <>)

removePango :: Block -> Block
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

modify :: (Block -> Block) -> Pipe Block Block IO ()
modify = PP.map

autoPadding :: Pipe Block Block IO ()
autoPadding = autoPadding' 0 0
  where
    autoPadding' :: Int64 -> Int64 -> Pipe Block Block IO ()
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
sharedInterval :: BarUpdateChannel -> Int -> IO (IO Block -> BlockProducer, Async ())
sharedInterval barUpdateChannel seconds = do
  clientsMVar <- newMVar ([] :: [(IO Block, Output Block)])

  task <- async $ forever $ do
    threadDelay $ seconds * 1000000
    -- Updates all client blocks
    -- If send returns 'False' the clients mailbox has been closed, so it is removed
    modifyMVar_ clientsMVar (fmap catMaybes . mapConcurrently runAndFilterClient)
    -- Then update the bar
    updateBar barUpdateChannel

  return (addClient clientsMVar, task)
    where
      runAndFilterClient :: (IO Block, Output Block) -> IO (Maybe (IO Block, Output Block))
      runAndFilterClient client = do
        result <- runClient client
        return $ if result then Just client else Nothing
      runClient :: (IO Block, Output Block) -> IO Bool
      runClient (blockAction, output) = do
        result <- blockAction
        atomically $ send output result {
          clickAction = Just (updateClickHandler result)
        }
        where
          updateClickHandler :: Block -> IO ()
          updateClickHandler block = do
            -- Give user feedback that the block is updating
            let outdatedBlock = setColor updatingColor $ removePango block
            void $ atomically $ send output $ outdatedBlock
            -- Notify bar about changed block state to display the feedback
            updateBar barUpdateChannel
            -- Run a normal block update to update the block to the new value
            void $ runClient (blockAction, output)
            -- Notify bar about changed block state, this is usually done by the shared interval handler
            updateBar barUpdateChannel
      addClient :: MVar [(IO Block, Output Block)] -> IO Block -> BlockProducer
      addClient clientsMVar blockAction = do
        -- Spawn the mailbox that preserves the latest block
        (output, input) <- lift $ spawn $ latest emptyBlock

        -- Generate initial block and send it to the mailbox
        lift $ void $ runClient (blockAction, output)

        -- Register the client for regular updates
        lift $ modifyMVar_ clientsMVar $ \ clients -> return ((blockAction, output):clients)

        -- Return a block producer from the mailbox
        fromInput input

blockScript :: FilePath -> IO Block
blockScript path = do
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
  where
    createScriptBlock :: T.Text -> Block
    createScriptBlock text = pangoMarkup $ setBlockName (T.pack path) $ createBlock text

startPersistentBlockScript :: BarUpdateChannel -> FilePath -> Producer Block IO ()
startPersistentBlockScript barUpdateChannel path = do
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
    signalFirstBlock :: Event.Event -> Pipe Block Block IO ()
    signalFirstBlock event = do
      -- Await first block
      await >>= yield
      lift $ Event.set event
      -- Replace with cat
      cat
    fromHandle :: Handle -> Producer Block IO ()
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
