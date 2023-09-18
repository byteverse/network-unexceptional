{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}

module Network.Unexceptional.ByteArray
  ( receiveExactly
  ) where

import Control.Exception (throwIO)
import Control.Monad (when)
import Data.Primitive (ByteArray)
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

-- | Blocks until an exact number of bytes has been received.
receiveExactly ::
     Socket
  -> Int -- ^ Exact number of bytes to receive, must be greater than zero
  -> IO (Either Errno ByteArray)
receiveExactly !s !n = do
  dst <- PM.newByteArray n
  MB.receiveExactly s (MutableBytes dst 0 n) >>= \case
    Left e -> pure (Left e)
    Right _ -> do
      dst' <- PM.unsafeFreezeByteArray dst
      pure (Right dst')
