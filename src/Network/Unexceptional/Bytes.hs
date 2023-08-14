{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}

module Network.Unexceptional.Bytes
  ( send
  , receive
  ) where

import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Array.Byte (ByteArray)
import Data.Bytes.Types (Bytes(Bytes),MutableBytes(MutableBytes))
import Foreign.C.Error (Errno)
import Foreign.C.Error.Pattern (pattern EWOULDBLOCK,pattern EAGAIN)
import GHC.Conc (threadWaitWrite)
import Network.Socket (Socket)
import System.Posix.Types (Fd(Fd))

import qualified Network.Unexceptional.Types as Types
import qualified Posix.Socket as X
import qualified Linux.Socket as X
import qualified Data.Bytes.Types
import qualified Network.Socket as S
import qualified Data.Primitive as PM
import qualified Network.Unexceptional.MutableBytes as MB

-- | Send the entire byte sequence. This call POSIX @send@ in a loop
-- until all of the bytes have been sent.
--
-- If this is passed the empty byte sequence, it doesn't actually call
-- POSIX @send()@. It just returns that it succeeded.
send ::
     Socket
  -> Bytes
  -> IO (Either Errno ())
send s Bytes{array,offset,length=len} = case len of
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
    Left e -> if e == EAGAIN || e == EWOULDBLOCK
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

-- | If this returns zero bytes, it means that the peer has
-- performed an orderly shutdown.
receive :: 
     Socket
  -> Int -- ^ Maximum number of bytes to receive
  -> IO (Either Errno Bytes)
receive s n = if n > 0
  then do
    dst <- PM.newByteArray n
    MB.receive s (MutableBytes dst 0 n) >>= \case
      Left e -> pure (Left e)
      Right m -> do
        when (m < n) (PM.shrinkMutableByteArray dst m)
        dst' <- PM.unsafeFreezeByteArray dst 
        pure (Right (Bytes dst' 0 m))
  else throwIO Types.NonpositiveReceptionSize


