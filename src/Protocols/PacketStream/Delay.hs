{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
Provides a circuit that delays a stream by a configurable amount of clock cycles.
-}
module Protocols.PacketStream.Delay (
  delayStream,
) where

import Clash.Prelude

import Protocols
import Protocols.PacketStream.Base

import Data.Maybe

-- TODO Optimization: _meta only needs to be buffered once because it is constant per packet
newtype DelayState cycles dataWidth meta = DelayState
  { _buf :: Vec cycles (Maybe (PacketStreamM2S dataWidth meta))
  -- ^ Transfer buffer
  }
  deriving (Generic, NFDataX, Show, ShowX)

-- | Forwards incoming packets with @cycles@ clock cycles latency.
delayStream ::
  forall (dom :: Domain) (dataWidth :: Nat) (meta :: Type) (cycles :: Nat).
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (1 <= cycles) =>
  (1 <= dataWidth) =>
  (NFDataX meta) =>
  SNat cycles ->
  Circuit (PacketStream dom dataWidth meta) (PacketStream dom dataWidth meta)
delayStream SNat = forceResetSanity |> fromSignals (mealyB go (DelayState @cycles (repeat Nothing)))
 where
  go st (fwdIn, bwdIn) = (nextStOut, (PacketStreamS2M outReady, fwdOut))
   where
    (newBuf, out) = shiftInAtN (_buf st) (singleton fwdIn)
    fwdOut = head out

    nextSt = DelayState newBuf

    (nextStOut, outReady)
      | isJust fwdOut && not (_ready bwdIn) = (st, False)
      | otherwise = (nextSt, True)
