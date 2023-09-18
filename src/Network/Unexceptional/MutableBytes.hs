{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}

module Network.Unexceptional.MutableBytes
  ( receive
  , receiveInterruptible
  , receiveExactly
  , receiveExactlyInterruptible
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent.STM (STM,TVar)
import Control.Exception (throwIO)
import Control.Monad ((<=<))
import Data.Bytes.Types (MutableBytes(MutableBytes))
import Data.Functor (($>))
import Data.Primitive (MutableByteArray)
import Foreign.C.Error (Errno)
import Foreign.C.Error.Pattern (pattern EWOULDBLOCK,pattern EAGAIN)
import Foreign.C.Error.Pattern (pattern EEOI)
import GHC.Conc (threadWaitRead,threadWaitReadSTM)
import GHC.Exts (RealWorld)
import Network.Socket (Socket)
import System.Posix.Types (Fd(Fd))

import qualified Control.Concurrent.STM as STM
import qualified Data.Bytes.Types
import qualified Linux.Socket as X
import qualified Network.Socket as S
import qualified Network.Unexceptional.Types as Types
import qualified Posix.Socket as X

-- | Receive bytes from a socket. Receives at most N bytes, where N
-- is the size of the buffer. Returns the number of bytes that were
-- actually received.
receive ::
     Socket
  -> MutableBytes RealWorld -- ^ Slice of a buffer
  -> IO (Either Errno Int)
receive s MutableBytes{array,offset,length=len} =
  if len > 0
    then S.withFdSocket s $ \fd ->
      -- We attempt the first receive without testing if the socket is
      -- ready for reads.
      receiveLoop (Fd fd) array offset len
    else throwIO Types.NonpositiveReceptionSize

receiveInterruptible ::
     TVar Bool -- ^ Interrupt
  -> Socket
  -> MutableBytes RealWorld -- ^ Slice of a buffer
  -> IO (Either Errno Int)
receiveInterruptible !interrupt s MutableBytes{array,offset,length=len} =
  if len > 0
    then S.withFdSocket s $ \fd ->
      -- We attempt the first receive without testing if the socket is
      -- ready for reads.
      receiveInterruptibleLoop interrupt (Fd fd) array offset len
    else throwIO Types.NonpositiveReceptionSize

-- Does not wait for file descriptor to be ready. Only performs
-- a single successful recv syscall
receiveLoop :: Fd -> MutableByteArray RealWorld -> Int -> Int -> IO (Either Errno Int)
receiveLoop !fd !arr !off !len =
  X.uninterruptibleReceiveMutableByteArray fd arr off (fromIntegral len) X.dontWait >>= \case
    Left e -> if e == EAGAIN || e == EWOULDBLOCK
      then do
        threadWaitRead fd
        receiveLoop fd arr off len
      else pure (Left e)
    Right recvSzC ->
      let recvSz = fromIntegral recvSzC :: Int
       in case recvSz of
            0 -> pure (Left EEOI)
            _ -> case compare recvSz len of
              GT -> throwIO Types.ReceivedTooManyBytes
              _ -> pure (Right recvSz)

-- Does not wait for file descriptor to be ready. Only performs
-- a single successful recv syscall
receiveInterruptibleLoop :: TVar Bool -> Fd -> MutableByteArray RealWorld -> Int -> Int -> IO (Either Errno Int)
receiveInterruptibleLoop !interrupt !fd !arr !off !len =
  X.uninterruptibleReceiveMutableByteArray fd arr off (fromIntegral len) X.dontWait >>= \case
    Left e -> if e == EAGAIN || e == EWOULDBLOCK
      then waitUntilReadable interrupt fd >>= \case
        Ready -> receiveInterruptibleLoop interrupt fd arr off len
        Interrupted -> pure (Left EAGAIN)
      else pure (Left e)
    Right recvSzC ->
      let recvSz = fromIntegral recvSzC :: Int
       in case recvSz of
            0 -> pure (Left EEOI)
            _ -> case compare recvSz len of
              GT -> throwIO Types.ReceivedTooManyBytes
              _ -> pure (Right recvSz)

checkFinished :: TVar Bool -> STM ()
checkFinished = STM.check <=< STM.readTVar

data Outcome = Ready | Interrupted

waitUntilReadable :: TVar Bool -> Fd -> IO Outcome
waitUntilReadable !interrupt !fd = do
  (isReadyAction,deregister) <- threadWaitReadSTM fd
  outcome <- STM.atomically $ (isReadyAction $> Ready) <|> (checkFinished interrupt $> Interrupted)
  deregister
  pure outcome

-- | Blocks until an exact number of bytes has been received.
receiveExactly ::
     Socket
  -> MutableBytes RealWorld
     -- ^ Length is the exact number of bytes to receive,
     -- must be greater than zero.
  -> IO (Either Errno ())
receiveExactly s (MutableBytes dst off0 n) = if n > 0
  then do
    let loop !ix !remaining = case remaining of
          0 -> pure (Right ())
          _ -> receive s (MutableBytes dst ix remaining) >>= \case
            Left e -> pure (Left e)
            Right k -> loop (ix + k) (remaining - k)
    loop off0 n
  else throwIO Types.NonpositiveReceptionSize

receiveExactlyInterruptible ::
     TVar Bool
  -> Socket
  -> MutableBytes RealWorld
     -- ^ Length is the exact number of bytes to receive,
     -- must be greater than zero.
  -> IO (Either Errno ())
receiveExactlyInterruptible !intr !s (MutableBytes dst off0 n) = if n > 0
  then do
    let loop !ix !remaining = case remaining of
          0 -> pure (Right ())
          _ -> receiveInterruptible intr s (MutableBytes dst ix remaining) >>= \case
            Left e -> pure (Left e)
            Right k -> loop (ix + k) (remaining - k)
    loop off0 n
  else throwIO Types.NonpositiveReceptionSize
