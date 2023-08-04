{-# language BangPatterns #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}

module Network.Unexceptional
  ( accept_
  ) where

import Network.Socket (Socket,mkSocket,withFdSocket)
import Foreign.C.Error (Errno)
import GHC.Conc (threadWaitRead)
import Foreign.C.Error.Pattern (pattern EWOULDBLOCK,pattern EAGAIN)
import System.Posix.Types (Fd(Fd))

import qualified Linux.Socket as X

-- | Accept a connection. See the documentation in @network@ for @accept@.
--
-- Note: This may leak a file descriptor if an asynchronous exception is
-- received while this function is running.
accept_ :: Socket -> IO (Either Errno Socket)
accept_ listing_sock = withFdSocket listing_sock $ \listing_fd -> do
  let acceptLoop = do
        threadWaitRead (Fd listing_fd)
        X.uninterruptibleAccept4_ (Fd listing_fd) (X.nonblocking <> X.closeOnExec) >>= \case
          Left e -> if e == EAGAIN || e == EWOULDBLOCK
            then acceptLoop
            else pure (Left e)
          Right (Fd fd) -> fmap Right (mkSocket fd)
  acceptLoop

