{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}

module Network.Unexceptional.ByteString
  ( send
  , sendInterruptible
  , receive
  , receiveExactly
  , receiveExactlyInterruptible
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent.STM (STM, TVar)
import Control.Monad (when, (<=<))
import Data.ByteString.Internal (ByteString (BS))
import Data.Bytes.Types (MutableBytes (MutableBytes))
import Data.Functor (($>))
import Data.Primitive (ByteArray (ByteArray))
import Data.Primitive.Addr (Addr (Addr), plusAddr)
import Foreign.C.Error (Errno)
import Foreign.C.Error.Pattern (pattern EAGAIN, pattern EWOULDBLOCK)
import GHC.Conc (threadWaitWrite, threadWaitWriteSTM)
import GHC.ForeignPtr (ForeignPtr (ForeignPtr), ForeignPtrContents (PlainPtr))
import Network.Socket (Socket)
import System.Posix.Types (Fd (Fd))

import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString.Unsafe as ByteString
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts
import qualified Linux.Socket as X
import qualified Network.Socket as S
import qualified Network.Unexceptional.MutableBytes as MB
import qualified Posix.Socket as X

{- | Send the entire byte sequence. This call POSIX @send@ in a loop
until all of the bytes have been sent.
-}
send ::
  Socket ->
  ByteString ->
  IO (Either Errno ())
send s !b =
  S.withFdSocket s $ \fd -> ByteString.unsafeUseAsCStringLen b $ \(PM.Ptr ptr, len) ->
    -- We attempt the first send without testing if the socket is in
    -- ready for writes. This is because it is uncommon for the transmit
    -- buffer to already be full.
    sendLoop (Fd fd) (Addr ptr) len

-- does not wait for file descriptor to be ready
sendLoop :: Fd -> Addr -> Int -> IO (Either Errno ())
sendLoop !fd !addr !len =
  X.uninterruptibleSend fd addr (fromIntegral len) (X.noSignal <> X.dontWait) >>= \case
    Left e ->
      if e == EAGAIN || e == EWOULDBLOCK
        then do
          threadWaitWrite fd
          sendLoop fd addr len
        else pure (Left e)
    Right sentSzC ->
      let sentSz = fromIntegral sentSzC :: Int
       in case compare sentSz len of
            EQ -> pure (Right ())
            LT -> sendLoop fd (plusAddr addr sentSz) (len - sentSz)
            GT -> fail "Network.Unexceptional.ByteString.sendLoop: send claimed to send too many bytes"

{- | Send the entire byte sequence. This call POSIX @send@ in a loop
until all of the bytes have been sent.
-}
sendInterruptible ::
  TVar Bool ->
  Socket ->
  ByteString ->
  IO (Either Errno ())
sendInterruptible !interrupt s !b =
  S.withFdSocket s $ \fd -> ByteString.unsafeUseAsCStringLen b $ \(PM.Ptr ptr, len) ->
    -- We attempt the first send without testing if the socket is in
    -- ready for writes. This is because it is uncommon for the transmit
    -- buffer to already be full.
    sendInterruptibleLoop interrupt (Fd fd) (Addr ptr) len

-- does not wait for file descriptor to be ready
sendInterruptibleLoop :: TVar Bool -> Fd -> Addr -> Int -> IO (Either Errno ())
sendInterruptibleLoop !interrupt !fd !addr !len =
  X.uninterruptibleSend fd addr (fromIntegral len) (X.noSignal <> X.dontWait) >>= \case
    Left e ->
      if e == EAGAIN || e == EWOULDBLOCK
        then
          waitUntilWriteable interrupt fd >>= \case
            Ready -> sendInterruptibleLoop interrupt fd addr len
            Interrupted -> pure (Left EAGAIN)
        else pure (Left e)
    Right sentSzC ->
      let sentSz = fromIntegral sentSzC :: Int
       in case compare sentSz len of
            EQ -> pure (Right ())
            LT -> sendInterruptibleLoop interrupt fd (plusAddr addr sentSz) (len - sentSz)
            GT -> fail "Network.Unexceptional.ByteString.sendInterruptibleLoop: send claimed to send too many bytes"

{- | If this returns zero bytes, it means that the peer has
performed an orderly shutdown.
-}
receive ::
  Socket ->
  -- | Maximum number of bytes to receive
  Int ->
  IO (Either Errno ByteString)
receive s n = do
  dst <- PM.newPinnedByteArray n
  MB.receive s (MutableBytes dst 0 n) >>= \case
    Left e -> pure (Left e)
    Right m -> do
      when (m < n) (PM.shrinkMutableByteArray dst m)
      ByteArray dst# <- PM.unsafeFreezeByteArray dst
      pure (Right (BS (ForeignPtr (Exts.byteArrayContents# dst#) (PlainPtr (Exts.unsafeCoerce# dst#))) m))

-- | Blocks until an exact number of bytes has been received.
receiveExactly ::
  Socket ->
  -- | Exact number of bytes to receive, must be greater than zero
  Int ->
  IO (Either Errno ByteString)
receiveExactly !s !n = do
  dst <- PM.newPinnedByteArray n
  MB.receiveExactly s (MutableBytes dst 0 n) >>= \case
    Left e -> pure (Left e)
    Right _ -> do
      ByteArray dst# <- PM.unsafeFreezeByteArray dst
      pure (Right (BS (ForeignPtr (Exts.byteArrayContents# dst#) (PlainPtr (Exts.unsafeCoerce# dst#))) n))

-- | Blocks until an exact number of bytes has been received.
receiveExactlyInterruptible ::
  TVar Bool ->
  Socket ->
  -- | Exact number of bytes to receive, must be greater than zero
  Int ->
  IO (Either Errno ByteString)
receiveExactlyInterruptible !intr !s !n = do
  dst <- PM.newPinnedByteArray n
  MB.receiveExactlyInterruptible intr s (MutableBytes dst 0 n) >>= \case
    Left e -> pure (Left e)
    Right _ -> do
      ByteArray dst# <- PM.unsafeFreezeByteArray dst
      pure (Right (BS (ForeignPtr (Exts.byteArrayContents# dst#) (PlainPtr (Exts.unsafeCoerce# dst#))) n))

waitUntilWriteable :: TVar Bool -> Fd -> IO Outcome
waitUntilWriteable !interrupt !fd = do
  (isReadyAction, deregister) <- threadWaitWriteSTM fd
  outcome <- STM.atomically $ (isReadyAction $> Ready) <|> (checkFinished interrupt $> Interrupted)
  deregister
  pure outcome

data Outcome = Ready | Interrupted

checkFinished :: TVar Bool -> STM ()
checkFinished = STM.check <=< STM.readTVar
