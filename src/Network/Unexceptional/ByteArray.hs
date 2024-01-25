{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Network.Unexceptional.ByteArray
  ( receiveExactly
  , receiveFromInterruptible
  ) where

import Control.Concurrent.STM (TVar)
import Data.Bytes.Types (MutableBytes (MutableBytes))
import Data.Primitive (ByteArray)
import Foreign.C.Error (Errno)
import Network.Socket (Socket)

import qualified Data.Primitive as PM
import qualified Network.Socket as S
import qualified Network.Unexceptional.MutableBytes as MB

-- | Blocks until an exact number of bytes has been received.
receiveExactly ::
  Socket ->
  -- | Exact number of bytes to receive, must be greater than zero
  Int ->
  IO (Either Errno ByteArray)
receiveExactly !s !n = do
  dst <- PM.newByteArray n
  MB.receiveExactly s (MutableBytes dst 0 n) >>= \case
    Left e -> pure (Left e)
    Right _ -> do
      dst' <- PM.unsafeFreezeByteArray dst
      pure (Right dst')

receiveFromInterruptible ::
  TVar Bool ->
  Socket ->
  -- | Maximum number of bytes to receive.
  Int ->
  IO (Either Errno (ByteArray, S.SockAddr))
receiveFromInterruptible !intr s !n = do
  dst <- PM.newByteArray n
  MB.receiveFromInterruptible intr s (MutableBytes dst 0 n) >>= \case
    Left err -> pure (Left err)
    Right (sz, sockAddr) -> do
      PM.shrinkMutableByteArray dst sz
      dst' <- PM.unsafeFreezeByteArray dst
      pure (Right (dst', sockAddr))
