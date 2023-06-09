module QBar.Blocks.Pipe (
  runPipeClient,
) where

import QBar.ControlSocket
import QBar.Core
import QBar.Prelude
import QBar.TagParser

import Control.Concurrent.Async
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 qualified as BSC
import Data.Text.Lazy qualified as T
import Pipes
import Pipes.Concurrent
import Pipes.Prelude qualified as PP
import System.IO

runPipeClient :: Bool -> MainOptions -> IO ()
runPipeClient enableEvents mainOptions = do
  (output, input) <- spawn unbounded
  hostTask <- async $ sendBlockStream (addBlock $ pipeBlock $ fromInput input) mainOptions
  inputTask <- async $ runEffect $ PP.stdinLn >-> toOutput output
  void $ waitEitherCancel hostTask inputTask
  where
    -- Special block that reads the processes stdin line-by-line and shows the latest line in the block. Must never be used in a 'server' process or when stdin/stdout is used in another way.
    pipeBlock :: Producer String BarIO () -> Block
    pipeBlock source = ExitBlock <$ source >-> pack
      where
        pack :: Pipe String BlockUpdate BarIO ()
        pack = forever $ do
          value <- await
          let output = parseTags' . T.pack $ value
          if enableEvents
            then pushBlockUpdate' handler output
            else pushBlockUpdate output

        handler :: BlockEventHandler
        handler event = liftIO $ BSC.hPutStrLn stdout $ encode event
