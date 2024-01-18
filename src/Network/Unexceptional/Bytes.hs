{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}

module Network.Unexceptional.Bytes
  ( send
  , sendInterruptible
  , receive
  , receiveInterruptible
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent.STM (STM, TVar)
import Control.Exception (throwIO)
import Control.Monad (when, (<=<))
import Data.Bytes.Types (Bytes (Bytes), MutableBytes (MutableBytes))
import Data.Functor (($>))
import Data.Primitive (ByteArray, MutableByteArray)
import Foreign.C.Error (Errno)
import Foreign.C.Error.Pattern (pattern EAGAIN, pattern EWOULDBLOCK)
import GHC.Conc (threadWaitWrite, threadWaitWriteSTM)
import GHC.Exts (RealWorld)
import Network.Socket (Socket)
import System.Posix.Types (Fd (Fd))

import qualified Control.Concurrent.STM as STM
import qualified Data.Bytes.Types
import qualified Data.Primitive as PM
import qualified Linux.Socket as X
import qualified Network.Socket as S
import qualified Network.Unexceptional.MutableBytes as MB
import qualified Network.Unexceptional.Types as Types
import qualified Posix.Socket as X

{- | Send the entire byte sequence. This call POSIX @send@ in a loop
until all of the bytes have been sent.

If this is passed the empty byte sequence, it doesn't actually call
POSIX @send()@. It just returns that it succeeded.
-}
send ::
  Socket ->
  Bytes ->
  IO (Either Errno ())
send s Bytes {array, offset, length = len} = case len of
  0 -> pure (Right ())
  _ -> S.withFdSocket s $ \fd ->
    -- We attempt the first send without testing if the socket is in
    -- ready for writes. This is because it is uncommon for the transmit
    -- buffer to already be full.
    sendLoop (Fd fd) array offset len

-- does not wait for file descriptor to be ready
sendLoop :: Fd -> ByteArray -> Int -> Int -> IO (Either Errno ())
sendLoop !fd !arr !off !len =
  X.uninterruptibleSendByteArray fd arr off (fromIntegral len) (X.noSignal <> X.dontWait) >>= \case
    Left e ->
      if e == EAGAIN || e == EWOULDBLOCK
        then do
          threadWaitWrite fd
          sendLoop fd arr off len
        else pure (Left e)
    Right sentSzC ->
      let sentSz = fromIntegral sentSzC :: Int
       in case compare sentSz len of
            EQ -> pure (Right ())
            LT -> sendLoop fd arr (off + sentSz) (len - sentSz)
            GT -> fail "Network.Unexceptional.Bytes.sendAll: send claimed to send too many bytes"

sendInterruptibleLoop :: TVar Bool -> Fd -> ByteArray -> Int -> Int -> IO (Either Errno ())
sendInterruptibleLoop !interrupt !fd !arr !off !len =
  X.uninterruptibleSendByteArray fd arr off (fromIntegral len) (X.noSignal <> X.dontWait) >>= \case
    Left e ->
      if e == EAGAIN || e == EWOULDBLOCK
        then
          waitUntilWriteable interrupt fd >>= \case
            Ready -> sendInterruptibleLoop interrupt fd arr off len
            Interrupted -> pure (Left EAGAIN)
        else pure (Left e)
    Right sentSzC ->
      let sentSz = fromIntegral sentSzC :: Int
       in case compare sentSz len of
            EQ -> pure (Right ())
            LT -> sendInterruptibleLoop interrupt fd arr (off + sentSz) (len - sentSz)
            GT -> fail "Network.Unexceptional.Bytes.sendAll: send claimed to send too many bytes"

-- | Variant of 'send' that fails with @EAGAIN@ if the interrupt ever becomes true.
sendInterruptible ::
  TVar Bool ->
  Socket ->
  Bytes ->
  IO (Either Errno ())
sendInterruptible !interrupt s Bytes {array, offset, length = len} = case len of
  0 -> pure (Right ())
  _ ->
    STM.readTVarIO interrupt >>= \case
      True -> pure (Left EAGAIN)
      False -> S.withFdSocket s $ \fd ->
        -- We attempt the first send without testing if the socket is in
        -- ready for writes. This is because it is uncommon for the transmit
        -- buffer to already be full.
        sendInterruptibleLoop interrupt (Fd fd) array offset len

data Outcome = Ready | Interrupted

checkFinished :: TVar Bool -> STM ()
checkFinished = STM.check <=< STM.readTVar

waitUntilWriteable :: TVar Bool -> Fd -> IO Outcome
waitUntilWriteable !interrupt !fd = do
  (isReadyAction, deregister) <- threadWaitWriteSTM fd
  outcome <- STM.atomically $ (isReadyAction $> Ready) <|> (checkFinished interrupt $> Interrupted)
  deregister
  pure outcome

{- | If this returns zero bytes, it means that the peer has
performed an orderly shutdown.
-}
receive ::
  Socket ->
  -- | Maximum number of bytes to receive
  Int ->
  IO (Either Errno Bytes)
receive s n =
  if n > 0
    then do
      dst <- PM.newByteArray n
      MB.receive s (MutableBytes dst 0 n) >>= handleRececeptionResult dst n
    else throwIO Types.NonpositiveReceptionSize

{- | If this returns zero bytes, it means that the peer has
performed an orderly shutdown.
-}
receiveInterruptible ::
  -- | Interrupt
  TVar Bool ->
  Socket ->
  -- | Maximum number of bytes to receive
  Int ->
  IO (Either Errno Bytes)
receiveInterruptible !interrupt s n =
  if n > 0
    then do
      dst <- PM.newByteArray n
      MB.receiveInterruptible interrupt s (MutableBytes dst 0 n) >>= handleRececeptionResult dst n
    else throwIO Types.NonpositiveReceptionSize

handleRececeptionResult :: MutableByteArray RealWorld -> Int -> Either Errno Int -> IO (Either Errno Bytes)
handleRececeptionResult !dst !n x = case x of
  Left e -> pure (Left e)
  Right m -> do
    when (m < n) (PM.shrinkMutableByteArray dst m)
    dst' <- PM.unsafeFreezeByteArray dst
    pure (Right (Bytes dst' 0 m))
