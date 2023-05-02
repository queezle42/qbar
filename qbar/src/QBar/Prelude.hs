module QBar.Prelude (
  module Prelude,
  (<=<),
  (>=>),
  ByteString.ByteString,
  Control.Monad.IO.Class.MonadIO,
  Control.Monad.IO.Class.liftIO,
  Control.Monad.forever,
  Control.Monad.unless,
  Control.Monad.void,
  Control.Monad.when,
  Maybe.listToMaybe,
  Text.Text,
  error,
  errorWithoutStackTrace,
  head,
  intercalate,
  trace,
  traceIO,
  traceId,
  traceM,
  traceShow,
  traceShowIO,
  traceShowId,
  traceShowIdIO,
  traceShowM,
  undefined,
) where

import Prelude hiding
  ( error,
    errorWithoutStackTrace,
    head,
    undefined,
  )
import qualified Prelude as P
import qualified Control.Monad
import Control.Monad ((>=>), (<=<))
import qualified Control.Monad.IO.Class
import qualified Data.ByteString as ByteString
import qualified Data.Maybe as Maybe
import qualified Data.Text.Lazy as Text
import qualified Debug.Trace as Trace
import qualified GHC.Stack.Types

{-# DEPRECATED head "Partial Function." #-}
head :: [a] -> a
head = P.head

{-# DEPRECATED error "Partial Function." #-}
error :: GHC.Stack.Types.HasCallStack => String -> a
error = P.error

{-# DEPRECATED errorWithoutStackTrace "Partial Function." #-}
errorWithoutStackTrace :: String -> a
errorWithoutStackTrace = P.errorWithoutStackTrace

{-# DEPRECATED undefined "Partial Function." #-}
undefined :: GHC.Stack.Types.HasCallStack => a
undefined = P.undefined

{-# DEPRECATED trace "Partitial Function." #-}
trace :: String -> a -> a
trace = Trace.trace

{-# DEPRECATED traceId "Partitial Function." #-}
traceId :: String -> String
traceId = Trace.traceId

{-# DEPRECATED traceShow "Partitial Function." #-}
traceShow :: Show a => a -> b -> b
traceShow = Trace.traceShow

{-# DEPRECATED traceShowId "Partitial Function." #-}
traceShowId :: Show a => a -> a
traceShowId = Trace.traceShowId

{-# DEPRECATED traceM "Partitial Function." #-}
traceM :: Applicative m => String -> m ()
traceM = Trace.traceM

{-# DEPRECATED traceShowM "Partitial Function." #-}
traceShowM :: (Show a, Applicative m) => a -> m ()
traceShowM = Trace.traceShowM

{-# DEPRECATED traceIO "Partitial Function." #-}
traceIO :: Control.Monad.IO.Class.MonadIO m => String -> m ()
traceIO = Control.Monad.IO.Class.liftIO . Trace.traceIO

{-# DEPRECATED traceShowIO "Partitial Function." #-}
traceShowIO :: (Control.Monad.IO.Class.MonadIO m, Show a) => a -> m ()
traceShowIO = traceIO . show

{-# DEPRECATED traceShowIdIO "Partitial Function." #-}
traceShowIdIO :: (Control.Monad.IO.Class.MonadIO m, Show a) => a -> m a
traceShowIdIO a = traceShowIO a >> return a

intercalate :: Monoid a => a -> [a] -> a
intercalate _ [] = mempty
intercalate _ [x] = x
intercalate inter (x : xs) = x <> inter <> intercalate inter xs
