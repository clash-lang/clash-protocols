{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

{-# OPTIONS -fplugin=Protocols.Plugin #-}

{- |
Provides a circuit that delays a stream by a configurable amount of transfers.
-}
module Protocols.PacketStream.Delay (
  delayStream,
  dropTailC,
) where

import Clash.Prelude

import Protocols
import qualified Protocols.Df as Df
import qualified Protocols.DfConv as DfConv
import Protocols.PacketStream.Base

import Data.Bifunctor
import Data.Data ((:~:) (Refl))
import Data.Maybe
import Data.Proxy

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

{- |
Gets a delayed @PacketStream@ as input together with a non-delayed
@DropTailInfo@, so that dropping can be done while correctly preserving
@_abort@ and adjusting @_last@.
-}
dropTailC' ::
  forall (dom :: Domain) (dataWidth :: Nat) (delayCycles :: Nat) (meta :: Type).
  (KnownDomain dom) =>
  (KnownNat dataWidth) =>
  (KnownNat delayCycles) =>
  (HiddenClockResetEnable dom) =>
  Circuit
    (PacketStream dom dataWidth meta, Df.Df dom (DropTailInfo dataWidth delayCycles))
    (PacketStream dom dataWidth meta)
dropTailC' = fromSignals (first unbundle . mealyB go (0, Nothing) . first bundle)
 where
  go (0, cache) ((fwdIn1, fwdIn2), bwdIn) = (nextStOut, ((bwdOut1, bwdOut2), fwdOut))
   where
    (bwdOut1, bwdOut2)
      | isNothing fwdOut = (PacketStreamS2M True, Ack True)
      | otherwise = (bwdIn, Ack (_ready bwdIn))

    (nextSt, fwdOut) = case (fwdIn1, fwdIn2) of
      (Nothing, Df.NoData) -> ((0, cache), Nothing)
      (Nothing, Df.Data inf) -> ((_transferDrop inf, cache), Nothing)
      (Just pkt, Df.NoData) -> case cache of
        Nothing -> ((0, Nothing), Just pkt)
        Just (abb, newIddx, delayy) -> ((delayy, Nothing), Just pkt{_abort = abb, _last = Just newIddx})
      (Just pkt, Df.Data inf) ->
        if _wait inf
          then ((0, Just (_aborted inf, _newIdx inf, _transferDrop inf)), Just pkt)
          else
            ( (_transferDrop inf, Nothing)
            , Just pkt{_last = Just (_newIdx inf), _abort = _aborted inf}
            )

    nextStOut = if isNothing fwdOut || _ready bwdIn then nextSt else (0, cache)
  go (st, cache) ((_, info), _) = ((nextSt, cache), ((PacketStreamS2M True, Ack True), Nothing))
   where
    nextSt = case info of
      Df.NoData -> st - 1
      Df.Data d -> _transferDrop d

-- | Information about the tail of a packet, for dropping purposes.
data DropTailInfo dataWidth delayCycles = DropTailInfo
  { _aborted :: Bool
  -- ^ Whether any fragment of the packet was aborted
  , _newIdx :: Index dataWidth
  -- ^ The adjusted byte enable
  , _transferDrop :: Index (delayCycles + 1)
  -- ^ The amount of transfers to drop from the tail
  , _wait :: Bool
  -- ^ Iff true, apply changes to transfer the next clock cycle instead
  }

{- |
Transmits information about a single packet upon seeing its last transfer.
-}
transmitDropInfoC ::
  forall (dom :: Domain) (n :: Nat) (dataWidth :: Nat) (delayCycles :: Nat) (meta :: Type).
  (KnownDomain dom) =>
  (KnownNat dataWidth) =>
  (KnownNat delayCycles) =>
  (1 <= dataWidth) =>
  (1 <= n) =>
  (HiddenClockResetEnable dom) =>
  SNat n ->
  Circuit
    (PacketStream dom dataWidth meta)
    (Df.Df dom (DropTailInfo dataWidth delayCycles))
transmitDropInfoC SNat = forceResetSanity |> fromSignals (mealyB go False)
 where
  go aborted (Nothing, Ack readyIn) = (aborted, (PacketStreamS2M readyIn, Df.NoData))
  go aborted (Just PacketStreamM2S{..}, Ack readyIn) = (nextAborted, (PacketStreamS2M readyIn, fwdOut))
   where
    x = natToNum @(n `Mod` dataWidth)
    abortOut = aborted || _abort

    (nextAborted, fwdOut) = case _last of
      Nothing -> (aborted || _abort, Df.NoData)
      Just i ->
        ( False
        , Df.Data
            DropTailInfo
              { _aborted = abortOut
              , _newIdx = satSub SatWrap i x
              , _transferDrop = case compareSNat (SNat @dataWidth) (SNat @n) of
                  SNatLE -> case sameNat d0 (SNat @(n `Mod` dataWidth)) of
                    Just Refl -> natToNum @(n `Div` dataWidth)
                    _ ->
                      if (resize i :: Index n) < natToNum @(n - dataWidth)
                        then natToNum @(n `DivRU` dataWidth)
                        else natToNum @(n `Div` dataWidth)
                  SNatGT -> if i >= natToNum @n then 0 else 1
              , _wait = case compareSNat (SNat @dataWidth) (SNat @n) of
                  SNatLE -> case sameNat d0 (SNat @(n `Mod` dataWidth)) of
                    Just Refl -> False
                    _ -> (resize i :: Index n) >= natToNum @(n - dataWidth)
                  SNatGT -> i >= natToNum @n
              }
        )

{- |
Removes the last @n@ bytes from each packet in a @PacketStream@.
If any dropped transfers had @_abort@ set, this will be preserved by
setting the @_abort@ of an earlier transfer that is not dropped.

__NB__: assumes that packets are at least @n@ bytes long. If this is
not the case, this component will break.
-}
dropTailC ::
  forall (dom :: Domain) (n :: Nat) (dataWidth :: Nat) (meta :: Type).
  (KnownDomain dom) =>
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  (1 <= n) =>
  (NFDataX meta) =>
  (HiddenClockResetEnable dom) =>
  SNat n ->
  Circuit
    (PacketStream dom dataWidth meta)
    (PacketStream dom dataWidth meta)
dropTailC SNat = case compareSNat d1 (SNat @(n `DivRU` dataWidth)) of
  SNatLE ->
    forceResetSanity
      |> circuit
        ( \stream -> do
            [s1, s2] <- DfConv.fanout Proxy Proxy -< stream
            delayed <- delayStream (SNat @(n `DivRU` dataWidth)) -< s1
            info <- transmitDropInfoC @dom @n @dataWidth @(n `DivRU` dataWidth) (SNat @n) -< s2
            dropTailC' @dom @dataWidth @(n `DivRU` dataWidth) @meta -< (delayed, info)
        )
  _ ->
    clashCompileError
      "dropTailC: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"
