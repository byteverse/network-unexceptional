# Revision history for network-unexceptional

## 0.1.3.1 -- 2023-08-31

* Import `ByteArray` from `Data.Primitive` instead of `Data.Array.Byte`
  to fix build failure on GHC 9.2.

## 0.1.3.0 -- 2023-08-30

* Add interruptible send and receive functions to give users control over
  when to give to abandon communicating.
* Add these functions to `Network.Unexceptional`: `connect`,
  `connectInterruptible`, `socket`.

## 0.1.1.0 -- 2023-08-14

* Add Network.Unexceptional module with `accept_`.
* Add Network.Unexceptional.ByteString module

## 0.1.0.0 -- YYYY-mm-dd

* First version.
