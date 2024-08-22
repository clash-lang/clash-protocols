{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
Provides a circuit that delays a stream by a configurable amount of transfers.
-}
module Protocols.PacketStream.Delay (
  delayStream,
) where

import Clash.Prelude

import Protocols
import Protocols.PacketStream.Base

import Data.Maybe

-- TODO Optimization: _meta only needs to be buffered once because it is constant per packet
-- Holds for _abort and _last too
data DelayState n dataWidth meta = DelayState
  { _buf :: Vec n (PacketStreamM2S dataWidth meta)
  -- ^ Transfer buffer
  , _size :: Index (n + 1)
  , _flush :: Bool
  , _readPtr :: Index n
  , _writePtr :: Index n
  }
  deriving (Generic, NFDataX, Show, ShowX)

-- | Forwards incoming packets with @n@ fragments latency.
delayStream ::
  forall (dom :: Domain) (dataWidth :: Nat) (meta :: Type) (n :: Nat).
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (1 <= n) =>
  (1 <= dataWidth) =>
  (NFDataX meta) =>
  -- | The number of fragments to delay
  SNat n ->
  Circuit (PacketStream dom dataWidth meta) (PacketStream dom dataWidth meta)
-- TODO this component is very unoptimized/ugly. The most important thing is
-- that it works now, but it should be improved in the future by removing.
-- dynamic indexing and perhaps using blockram.
delayStream SNat =
  forceResetSanity
    |> fromSignals
      (mealyB go (DelayState @n (repeat (errorX "undefined initial contents")) 0 False 0 0))
 where
  go st@DelayState{..} (Nothing, bwdIn) = (nextStOut, (bwdOut, fwdOut))
   where
    out = _buf !! _readPtr

    readEn = _size > 0 && _flush

    fwdOut =
      if readEn
        then Just out
        else Nothing

    bwdOut = PacketStreamS2M True

    nextSt =
      st
        { _size = _size - 1
        , _flush = _size - 1 > 0
        , _readPtr = satSucc SatWrap _readPtr
        }
    nextStOut = if readEn && _ready bwdIn then nextSt else st
  go st@DelayState{..} (Just inPkt, bwdIn) = (nextStOut, (bwdOut, fwdOut))
   where
    readEn = _flush || _size == maxBound

    out = _buf !! _readPtr
    newBuf = replace _writePtr inPkt _buf

    fwdOut =
      if readEn
        then Just out
        else Nothing

    bwdOut = PacketStreamS2M $ not _flush && (not readEn || _ready bwdIn)

    nextReadPtr = if readEn then satSucc SatWrap _readPtr else _readPtr

    (nextBuf, nextSize, nextFlush, nextWritePtr)
      | _flush = (_buf, satPred SatBound _size, satPred SatBound _size > 0, _writePtr)
      | otherwise =
          (newBuf, satSucc SatBound _size, isJust (_last inPkt), satSucc SatWrap _writePtr)

    nextSt = DelayState nextBuf nextSize nextFlush nextReadPtr nextWritePtr
    nextStOut = if isNothing fwdOut || _ready bwdIn then nextSt else st
