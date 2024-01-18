{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Network.Unexceptional
  ( accept_
  , socket
  , connect
  , connectInterruptible
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent.STM (STM, TVar)
import Control.Exception (mask_)
import Control.Monad ((<=<))
import Data.Functor (($>))
import Foreign.C.Error (Errno (Errno))
import Foreign.C.Error.Pattern (pattern EAGAIN, pattern EINPROGRESS, pattern EINTR, pattern EWOULDBLOCK)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (castPtr, nullPtr)
import GHC.Conc (threadWaitRead, threadWaitWrite, threadWaitWriteSTM)
import GHC.Exts (Ptr)
import Network.Socket (SockAddr, Socket, SocketOption (SoError), getSocketOption, mkSocket, withFdSocket)
import Network.Socket.Address (SocketAddress, pokeSocketAddress, sizeOfSocketAddress)
import System.Posix.Types (Fd (Fd))

import qualified Control.Concurrent.STM as STM
import qualified Linux.Socket as X
import qualified Network.Socket as N
import qualified Posix.Socket as X

{- | Accept a connection. See the documentation in @network@ for @accept@.

Note: This may leak a file descriptor if an asynchronous exception is
received while this function is running.
-}
accept_ :: Socket -> IO (Either Errno Socket)
accept_ listing_sock = withFdSocket listing_sock $ \listing_fd -> do
  let acceptLoop = do
        threadWaitRead (Fd listing_fd)
        X.uninterruptibleAccept4_ (Fd listing_fd) (X.nonblocking <> X.closeOnExec) >>= \case
          Left e ->
            if e == EAGAIN || e == EWOULDBLOCK
              then acceptLoop
              else pure (Left e)
          Right (Fd fd) -> fmap Right (mkSocket fd)
  acceptLoop

{- | Connect to a socket address. See the documentation in @network@
for @connect@.
-}
connect :: Socket -> SockAddr -> IO (Either Errno ())
connect s sa = withSocketAddress sa $ \p_sa sz -> withFdSocket s $ \fd ->
  let loop = do
        r <- X.uninterruptibleConnectPtr (Fd fd) p_sa sz
        case r of
          Right _ -> pure (Right ())
          Left err -> case err of
            EINTR -> loop
            EINPROGRESS -> do
              threadWaitWrite (Fd fd)
              errB <- getSocketOption s SoError
              case errB of
                0 -> pure (Right ())
                _ -> pure (Left (Errno (fromIntegral errB)))
            _ -> pure (Left err)
   in loop

{- | Variant of 'connect' that can be interrupted by setting the interrupt
variable to @True@. If interrupted in this way, this function returns
@EAGAIN@. For example, to attempt to connect for no more than 1 second:

> interrupt <- Control.Concurrent.STM.registerDelay 1_000_000
> connectInterruptible interrupt sock sockAddr
-}
connectInterruptible :: TVar Bool -> Socket -> SockAddr -> IO (Either Errno ())
connectInterruptible !interrupt s sa = withSocketAddress sa $ \p_sa sz -> withFdSocket s $ \fd ->
  let loop = do
        r <- X.uninterruptibleConnectPtr (Fd fd) p_sa sz
        case r of
          Right _ -> pure (Right ())
          Left err -> case err of
            EINTR -> loop
            EINPROGRESS ->
              waitUntilWriteable interrupt (Fd fd) >>= \case
                Interrupted -> pure (Left EAGAIN)
                Ready -> do
                  errB <- getSocketOption s SoError
                  case errB of
                    0 -> pure (Right ())
                    _ -> pure (Left (Errno (fromIntegral errB)))
            _ -> pure (Left err)
   in loop

-- Copied this from the network library. TODO: See if network can
-- just export this.
withSocketAddress :: (SocketAddress sa) => sa -> (Ptr sa -> Int -> IO a) -> IO a
withSocketAddress addr f = do
  let sz = sizeOfSocketAddress addr
  if sz == 0
    then f nullPtr 0
    else allocaBytes sz $ \p -> pokeSocketAddress p addr >> f (castPtr p) sz

data Outcome = Ready | Interrupted

checkFinished :: TVar Bool -> STM ()
checkFinished = STM.check <=< STM.readTVar

waitUntilWriteable :: TVar Bool -> Fd -> IO Outcome
waitUntilWriteable !interrupt !fd = do
  (isReadyAction, deregister) <- threadWaitWriteSTM fd
  outcome <- STM.atomically $ (isReadyAction $> Ready) <|> (checkFinished interrupt $> Interrupted)
  deregister
  pure outcome

{- | Create a socket. See the documentation in @network@ for @socket@.

There is no interruptible variant of this function because it cannot
block. (It does not actually perform network activity.)
-}
socket ::
  N.Family -> -- Family Name (usually AF_INET)
  N.SocketType -> -- Socket Type (usually Stream)
  N.ProtocolNumber -> -- Protocol Number (getProtocolByName to find value)
  IO (Either Errno Socket) -- Unconnected Socket
socket !fam !stype !protocol = case stype of
  N.Stream -> finish X.stream
  N.Datagram -> finish X.datagram
  _ -> fail "Network.Unexceptional.socket: Currently only supports stream and datagram types"
 where
  finish !sockTy = mask_ $ do
    X.uninterruptibleSocket (X.Family (N.packFamily fam)) (X.applySocketFlags (X.closeOnExec <> X.nonblocking) sockTy) (X.Protocol protocol) >>= \case
      Left err -> pure (Left err)
      Right (Fd fd) -> do
        s <- mkSocket fd
        pure (Right s)
