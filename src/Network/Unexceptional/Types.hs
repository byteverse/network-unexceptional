{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

{- | All of the exceptions defined in this module indicate either misuse
of the library or an implementation mistake in the libary.
-}
module Network.Unexceptional.Types
  ( NonpositiveReceptionSize (..)
  , ReceivedTooManyBytes (..)
  ) where

import Control.Exception (Exception)

{- | Thrown when any of the @receive@ functions are called with
a length less than 1. This includes zero and any negative numbers.
This indicates misuse of the API and is not considered a recoverable
exception.

Requesting a negative number is bytes is clear misuse of the API.
But what about zero? This deserves some justification. POSIX allows
requesting zero bytes with @recv@, and the result is that it copies
no bytes into the buffer and returns 0. Essentially, it's a no-op.
However, the return length 0 is also used to indicate a shutdown.
This overloaded meaning of the return value 0 makes it difficult to
interpret what it means. (It would be nice if @recv@ instead set the
error code to something indicating EOF when the peer had shutdown,
but we live in a more difficult world.) To correctly interpret the
meaning of return length 0, an application must consider what buffer
size it passed to @recv@. To prevent the caller from having to do this
bookkeeping, this library simply forbids requesting 0 bytes with @recv@.
If you do request 0 bytes with @recv@, you get this exception, and you
can fix the part of your program that failed to satisfy the
precondition.
-}
data NonpositiveReceptionSize = NonpositiveReceptionSize
  deriving stock (Show)
  deriving anyclass (Exception)

{- | This indicates a mistake in this library. Open an issue if this
exception is ever thrown.
-}
data ReceivedTooManyBytes = ReceivedTooManyBytes
  deriving stock (Show)
  deriving anyclass (Exception)
