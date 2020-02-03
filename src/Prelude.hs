{-# LANGUAGE NoImplicitPrelude #-}

module Prelude
  ( module BasePrelude,
    head,
    error,
    errorWithoutStackTrace,
    undefined,
    Text.Text,
    ByteString.ByteString,
    trace,
    traceId,
    traceShow,
    traceShowId,
    Maybe.listToMaybe,
  )
where

import BasePrelude hiding
  ( error,
    errorWithoutStackTrace,
    head,
    undefined,
  )
import qualified BasePrelude as P
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
