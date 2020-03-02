{-# LANGUAGE NoImplicitPrelude #-}

module Prelude
  ( module BasePrelude,
    ByteString.ByteString,
    (>=>),
    (<=<),
    Control.Monad.forever,
    Control.Monad.unless,
    Control.Monad.void,
    Control.Monad.when,
    Control.Monad.IO.Class.MonadIO,
    Control.Monad.IO.Class.liftIO,
    Text.Text,
    Maybe.listToMaybe,
    error,
    errorWithoutStackTrace,
    head,
    intercalate,
    trace,
    traceId,
    traceShow,
    traceShowId,
    traceM,
    traceShowM,
    undefined,
  )
where

import BasePrelude hiding
  ( error,
    errorWithoutStackTrace,
    head,
    undefined,
  )
import qualified BasePrelude as P
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

intercalate :: Monoid a => a -> [a] -> a
intercalate _ [] = mempty
intercalate _ [x] = x
intercalate inter (x : xs) = x <> inter <> intercalate inter xs
