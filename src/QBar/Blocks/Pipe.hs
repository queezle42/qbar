module QBar.Blocks.Pipe where

import QBar.BlockOutput
import QBar.ControlSocket
import QBar.Core

import Control.Concurrent.Async
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.Text.Lazy as T
import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as PP
import System.IO

runPipeClient :: Bool -> MainOptions -> IO ()
runPipeClient enableEvents mainOptions = do
  (output, input) <- spawn unbounded
  hostTask <- async $ sendBlockStream (addBlock $ pipeBlock $ fromInput input) mainOptions
  inputTask <- async $ runEffect $ PP.stdinLn >-> toOutput output
  void $ waitEitherCancel hostTask inputTask
  where
    -- |Special block that reads the processes stdin line-by-line and shows the latest line in the block. Must never be used in a 'server' process or when stdin/stdout is used in another way.
    pipeBlock :: Producer String BarIO () -> PushBlock
    pipeBlock source = PushMode <$ source >-> PP.map stringToState
      where
        stringToState :: String -> BlockState
        stringToState = attachHandler . mkBlockOutput . normalText . T.pack
        attachHandler :: BlockOutput -> BlockState
        attachHandler = if enableEvents then mkBlockState' pipeBlockName handler else mkBlockState
        handler :: BlockEventHandler
        handler event = liftIO $ BSC.hPutStrLn stderr $ encode event
        pipeBlockName :: Text
        pipeBlockName = "pipe"
