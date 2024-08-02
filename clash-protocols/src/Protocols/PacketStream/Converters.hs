{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
Provides an upconverter and downconverter for changing the data width of packet streams.
-}
module Protocols.PacketStream.Converters (
  upConverterC,
  downConverterC,
) where

import Clash.Prelude

import Protocols (Circuit (..), fromSignals, idC, (|>))
import Protocols.PacketStream.Base

import Data.Data ((:~:) (Refl))
import Data.Maybe (isJust)
import Data.Maybe.Extra

-- | Upconverter state, consisting of at most p `BitVector 8`s and a vector indicating which bytes are valid
data UpConverterState (dwOut :: Nat) (n :: Nat) (meta :: Type) = UpConverterState
  { _ucBuf :: Vec dwOut (BitVector 8)
  -- ^ The buffer we are filling
  , _ucIdx :: Index n
  -- ^ Where in the buffer we need to write the next element
  , _ucFlush :: Bool
  -- ^ If this is true the current state can presented as packetstream word
  , _ucFreshBuf :: Bool
  -- ^ If this is true we need to start a fresh buffer
  , _ucAborted :: Bool
  -- ^ Current packet is aborted
  , _ucLastIdx :: Maybe (Index dwOut)
  -- ^ If true the current buffer contains the last byte of the current packet
  , _ucMeta :: meta
  }
  deriving (Generic, NFDataX)

toPacketStream :: UpConverterState dwOut n meta -> Maybe (PacketStreamM2S dwOut meta)
toPacketStream UpConverterState{..} = toMaybe _ucFlush (PacketStreamM2S _ucBuf _ucLastIdx _ucMeta _ucAborted)

nextState ::
  forall (dwIn :: Nat) (dwOut :: Nat) (meta :: Type) (n :: Nat).
  (1 <= dwIn) =>
  (1 <= dwOut) =>
  (1 <= n) =>
  (KnownNat dwIn) =>
  (KnownNat dwOut) =>
  (KnownNat n) =>
  (NFDataX meta) =>
  (dwOut ~ dwIn * n) =>
  UpConverterState dwOut n meta ->
  Maybe (PacketStreamM2S dwIn meta) ->
  PacketStreamS2M ->
  UpConverterState dwOut n meta
nextState st@(UpConverterState{..}) Nothing (PacketStreamS2M inReady) =
  nextSt
 where
  outReady = not _ucFlush || inReady
  -- If we can accept data we can always set _ucFlush to false,
  -- since we only change state if we can transmit and receive data
  nextStRaw =
    st
      { _ucFlush = False
      , _ucAborted = not _ucFlush && _ucAborted
      , _ucLastIdx = Nothing
      }
  nextSt = if outReady then nextStRaw else st
nextState st@(UpConverterState{..}) (Just PacketStreamM2S{..}) (PacketStreamS2M inReady) =
  nextSt
 where
  nextAbort = (not _ucFlush && _ucAborted) || _abort
  -- If we are not flushing we can accept data to be stored in _ucBuf,
  -- but when we are flushing we can only accept if the current
  -- output fragment is accepted by the sink
  outReady = not _ucFlush || inReady
  bufFull = _ucIdx == maxBound
  currBuf = if _ucFreshBuf then repeat 0 else _ucBuf

  nextBuf =
    bitCoerce
      $ replace
        _ucIdx
        (bitCoerce _data :: BitVector (8 * dwIn))
        (bitCoerce currBuf :: Vec n (BitVector (8 * dwIn)))

  nextFlush = isJust _last || bufFull
  nextIdx = if nextFlush then 0 else _ucIdx + 1

  nextStRaw =
    UpConverterState
      { _ucBuf = nextBuf
      , _ucIdx = nextIdx
      , _ucFlush = nextFlush
      , _ucFreshBuf = nextFlush
      , _ucAborted = nextAbort
      , _ucLastIdx = (\i -> resize _ucIdx * natToNum @dwIn + resize i) <$> _last
      , _ucMeta = _meta
      }
  nextSt = if outReady then nextStRaw else st

{- | Converts packet streams of single bytes to packet streams of a higher data widths.
Has one cycle of latency, but optimal throughput.
-}
upConverterC ::
  forall (dwIn :: Nat) (dwOut :: Nat) (meta :: Type) (dom :: Domain) (n :: Nat).
  (HiddenClockResetEnable dom) =>
  (1 <= dwIn) =>
  (1 <= dwOut) =>
  (1 <= n) =>
  (KnownNat dwIn) =>
  (KnownNat dwOut) =>
  (KnownNat n) =>
  (dwOut ~ dwIn * n) =>
  (NFDataX meta) =>
  Circuit (PacketStream dom dwIn meta) (PacketStream dom dwOut meta)
upConverterC = case sameNat (SNat @dwIn) (SNat @dwOut) of
  Just Refl -> idC
  _ -> forceResetSanity |> fromSignals (mealyB go s0)
   where
    s0 = UpConverterState (repeat undefined) 0 False True False Nothing undefined
    go st@(UpConverterState{..}) (fwdIn, bwdIn) =
      (nextState st fwdIn bwdIn, (PacketStreamS2M outReady, toPacketStream st))
     where
      outReady = not _ucFlush || _ready bwdIn

data DownConverterState (dwIn :: Nat) = DownConverterState
  { _dcBuf :: Vec dwIn (BitVector 8)
  -- ^ Buffer
  , _dcSize :: Index (dwIn + 1)
  -- ^ Number of valid bytes in _dcBuf
  }
  deriving (Generic, NFDataX)

downConverterT ::
  forall (dwIn :: Nat) (dwOut :: Nat) (meta :: Type) (n :: Nat).
  (1 <= dwIn) =>
  (1 <= dwOut) =>
  (1 <= n) =>
  (KnownNat dwIn) =>
  (KnownNat dwOut) =>
  (dwIn ~ dwOut * n) =>
  DownConverterState dwIn ->
  (Maybe (PacketStreamM2S dwIn meta), PacketStreamS2M) ->
  ( DownConverterState dwIn
  , (PacketStreamS2M, Maybe (PacketStreamM2S dwOut meta))
  )
downConverterT st (Nothing, _) = (st, (PacketStreamS2M True, Nothing))
downConverterT st@DownConverterState{..} (Just inPkt, bwdIn) = (nextSt, (PacketStreamS2M outReady, Just outPkt))
 where
  -- If _dcSize == 0, then we have received a new transfer and should use
  -- its corresponding _data. Else, we should use our stored buffer.
  (nextSize, buf) = case (_dcSize == 0, _last inPkt) of
    (True, Nothing) -> (maxBound - natToNum @dwOut, _data inPkt)
    (True, Just i) -> (satSub SatBound (resize i + 1) (natToNum @dwOut), _data inPkt)
    (False, _) -> (satSub SatBound _dcSize (natToNum @dwOut), _dcBuf)

  (newBuf, dataOut) = leToPlus @dwOut @dwIn shiftOutFrom0 (SNat @dwOut) buf

  outPkt =
    PacketStreamM2S
      { _data = dataOut
      , _last = outLast
      , _meta = _meta inPkt
      , _abort = _abort inPkt
      }

  (outReady, outLast)
    | nextSize == 0 =
        (_ready bwdIn, resize . (\i -> i `mod` natToNum @dwOut) <$> _last inPkt)
    | otherwise = (False, Nothing)

  -- Keep the buffer in the state and rotate it once the byte is acknowledged to avoid
  -- dynamic indexing.
  nextSt
    | _ready bwdIn = DownConverterState newBuf nextSize
    | otherwise = st

{- | Converts packet streams of arbitrary data width `dwIn` to packet streams of
a smaller data width, `dwOut`, where `dwOut` must divide `dwIn`.

If @_abort@ is asserted on an input transfer, it will be asserted on all
corresponding output transfers as well.

Provides zero latency and optimal throughput, i.e. a packet of n bytes is
sent out in n clock cycles, even if `_last` is set.
-}
downConverterC ::
  forall (dwIn :: Nat) (dwOut :: Nat) (meta :: Type) (dom :: Domain) (n :: Nat).
  (HiddenClockResetEnable dom) =>
  (1 <= dwIn) =>
  (1 <= dwOut) =>
  (1 <= n) =>
  (KnownNat dwIn) =>
  (KnownNat dwOut) =>
  (dwIn ~ dwOut * n) =>
  Circuit (PacketStream dom dwIn meta) (PacketStream dom dwOut meta)
downConverterC = case sameNat (SNat @dwIn) (SNat @dwOut) of
  Just Refl -> idC
  _ -> forceResetSanity |> fromSignals (mealyB downConverterT s0)
   where
    s0 =
      DownConverterState
        { _dcBuf = errorX "downConverterC: undefined initial value"
        , _dcSize = 0
        }
