module QBar.Blocks.Pipe where

import QBar.BlockOutput
import QBar.Core

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.Text.Lazy as T
import Pipes
import qualified Pipes.Prelude as PP
import System.IO

-- |Special block that reads the processes stdin line-by-line and shows the latest line in the block. Must never be used in a 'server' process or when stdin/stdout is used in another way.
pipeBlock :: Bool -> PushBlock
pipeBlock enableEvents = PushMode <$ PP.stdinLn >-> PP.map stringToState
  where
    stringToState :: String -> BlockState
    stringToState = attachHandler . mkBlockOutput . normalText . T.pack
    attachHandler :: BlockOutput -> BlockState
    attachHandler = if enableEvents then mkBlockState' pipeBlockName handler else mkBlockState
    handler :: BlockEventHandler
    handler event = liftIO $ BSC.hPutStrLn stderr $ encode event
    pipeBlockName :: Text
    pipeBlockName = "pipe"
