{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language MagicHash #-}
{-# language PatternSynonyms #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}

module Network.Unexceptional.ByteString
  ( send
  , receive
  ) where

import Control.Monad (when)
import Data.ByteString.Internal (ByteString(BS))
import Data.Bytes.Types (MutableBytes(MutableBytes))
import Data.Primitive (ByteArray(ByteArray))
import Data.Primitive.Addr (Addr(Addr),plusAddr)
import Foreign.C.Error (Errno)
import Foreign.C.Error.Pattern (pattern EWOULDBLOCK,pattern EAGAIN)
import GHC.Conc (threadWaitWrite)
import GHC.ForeignPtr (ForeignPtr(ForeignPtr),ForeignPtrContents(PlainPtr))
import Network.Socket (Socket)
import System.Posix.Types (Fd(Fd))

import qualified Data.ByteString.Unsafe as ByteString
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts
import qualified Linux.Socket as X
import qualified Network.Socket as S
import qualified Network.Unexceptional.MutableBytes as MB
import qualified Posix.Socket as X

-- | Send the entire byte sequence. This call POSIX @send@ in a loop
-- until all of the bytes have been sent.
send ::
     Socket
  -> ByteString
  -> IO (Either Errno ())
send s !b =
  S.withFdSocket s $ \fd -> ByteString.unsafeUseAsCStringLen b $ \(PM.Ptr ptr,len) ->
  -- We attempt the first send without testing if the socket is in
  -- ready for writes. This is because it is uncommon for the transmit
  -- buffer to already be full.
  sendLoop (Fd fd) (Addr ptr) len

-- does not wait for file descriptor to be ready
sendLoop :: Fd -> Addr -> Int -> IO (Either Errno ())
sendLoop !fd !addr !len =
  X.uninterruptibleSend fd addr (fromIntegral len) (X.noSignal <> X.dontWait) >>= \case
    Left e -> if e == EAGAIN || e == EWOULDBLOCK
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

-- | If this returns zero bytes, it means that the peer has
-- performed an orderly shutdown.
receive :: 
     Socket
  -> Int -- ^ Maximum number of bytes to receive
  -> IO (Either Errno ByteString)
receive s n = do
  dst <- PM.newPinnedByteArray n
  MB.receive s (MutableBytes dst 0 n) >>= \case
    Left e -> pure (Left e)
    Right m -> do
      when (m < n) (PM.shrinkMutableByteArray dst m)
      ByteArray dst# <- PM.unsafeFreezeByteArray dst 
      pure (Right (BS (ForeignPtr (Exts.byteArrayContents# dst#) (PlainPtr (Exts.unsafeCoerce# dst#))) m))

