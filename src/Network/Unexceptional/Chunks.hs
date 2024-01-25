{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Network.Unexceptional.Chunks
  ( send
  , sendInterruptible
  ) where

import Control.Concurrent.STM (TVar)
import Data.Bytes.Chunks (Chunks)
import Foreign.C.Error (Errno)
import Network.Socket (Socket)

import qualified Data.Bytes.Chunks as Chunks
import qualified Network.Unexceptional.Bytes as NB

{- | Send the entire byte sequence. This call POSIX @send@ in a loop
until all of the bytes have been sent.
-}
send ::
  Socket ->
  Chunks ->
  IO (Either Errno ())
send !s cs = NB.send s (Chunks.concat cs)

sendInterruptible ::
  TVar Bool ->
  Socket ->
  Chunks ->
  IO (Either Errno ())
sendInterruptible !interrupt !s cs = NB.sendInterruptible interrupt s (Chunks.concat cs)
