{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}

module Network.Unexceptional.MutableBytes
  ( receive
  ) where

import Data.Bytes.Types (MutableBytes(MutableBytes))
import Data.Primitive (MutableByteArray)
import Foreign.C.Error (Errno)
import Foreign.C.Error.Pattern (pattern EWOULDBLOCK,pattern EAGAIN)
import GHC.Conc (threadWaitRead)
import GHC.Exts (RealWorld)
import Network.Socket (Socket)
import System.Posix.Types (Fd(Fd))

import qualified Data.Bytes.Types
import qualified Linux.Socket as X
import qualified Network.Socket as S
import qualified Posix.Socket as X

-- | Receive bytes from a socket. Receives at most N bytes, where N
-- is the size of the buffer. Returns the number of bytes that were
-- actually received.
receive ::
     Socket
  -> MutableBytes RealWorld -- ^ Slice of a buffer
  -> IO (Either Errno Int)
receive s MutableBytes{array,offset,length=len} = S.withFdSocket s $ \fd ->
  -- We attempt the first send without testing if the socket is in
  -- ready for writes. This is because it is uncommon for the transmit
  -- buffer to already be full.
  receiveLoop (Fd fd) array offset len

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
       in case compare recvSz len of
            GT -> fail "Network.Unexceptional.Bytes.receive: recv claimed to receive too many bytes"
            _ -> pure (Right recvSz)
