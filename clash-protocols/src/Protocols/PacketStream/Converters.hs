{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
Provides an upconverter and downconverter for changing the data width of packet streams.
-}
module Protocols.PacketStream.Converters (
  downConverterC,
  upConverterC,
  unsafeUpConverterC,
) where

import Clash.Prelude

import Data.Maybe (isJust)
import Data.Maybe.Extra
import Data.Type.Equality ((:~:) (Refl))

import Protocols (CSignal, Circuit (..), fromSignals, idC, (|>))
import Protocols.PacketStream.Base

-- | Upconverter state, consisting of at most p `BitVector 8`s and a vector indicating which bytes are valid
data UpConverterState (dwOut :: Nat) (n :: Nat) (meta :: Type) = UpConverterState
  { _ucBuf :: Vec dwOut (BitVector 8)
  -- ^ The buffer we are filling
  , _ucIdx :: Index n
  -- ^ Where in the buffer we need to write the next element
  , _ucIdx2 :: Index (dwOut + 1)
  -- ^ Used when @dwIn@ is not a power of two to determine the adjusted '_last',
  --   to avoid multiplication (infers an expensive DSP slice).
  --   If @dwIn@ is a power of two then we can multiply by shifting left.
  , _ucFlush :: Bool
  -- ^ If this is true the current state can presented as packetstream word
  , _ucFreshBuf :: Bool
  -- ^ If this is true we need to start a fresh buffer
  , _ucAborted :: Bool
  -- ^ Current packet is aborted
  , _ucLastIdx :: Maybe (Index (dwOut + 1))
  -- ^ If true the current buffer contains the last byte of the current packet
  , _ucMeta :: meta
  }
  deriving (Generic, NFDataX, Show, ShowX)

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

  -- If @dwIn@ is not a power of two, we need to do some extra bookkeeping to
  -- avoid multiplication. If not, _ucIdx2 stays at 0 and is never used, and
  -- should therefore be optimized out by synthesis tools.
  (nextIdx2, nextLastIdx) = case sameNat (SNat @(FLog 2 dwIn)) (SNat @(CLog 2 dwIn)) of
    Just Refl ->
      ( 0
      , (\i -> shiftL (resize _ucIdx) (natToNum @(Log 2 dwIn)) + resize i) <$> _last
      )
    Nothing ->
      ( if nextFlush then 0 else _ucIdx2 + natToNum @dwIn
      , (\i -> _ucIdx2 + resize i) <$> _last
      )

  nextStRaw =
    UpConverterState
      { _ucBuf = nextBuf
      , _ucIdx = nextIdx
      , _ucIdx2 = nextIdx2
      , _ucFlush = nextFlush
      , _ucFreshBuf = nextFlush
      , _ucAborted = nextAbort
      , _ucLastIdx = nextLastIdx
      , _ucMeta = _meta
      }
  nextSt = if outReady then nextStRaw else st

upConverter ::
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
  ( Signal dom (Maybe (PacketStreamM2S dwIn meta))
  , Signal dom PacketStreamS2M
  ) ->
  ( Signal dom PacketStreamS2M
  , Signal dom (Maybe (PacketStreamM2S dwOut meta))
  )
upConverter = mealyB go s0
 where
  s0 =
    UpConverterState
      { _ucBuf = deepErrorX "upConverterC: undefined initial buffer"
      , _ucIdx = 0
      , _ucIdx2 = 0
      , _ucFlush = False
      , _ucFreshBuf = True
      , _ucAborted = False
      , _ucLastIdx = Nothing
      , _ucMeta = deepErrorX "upConverterC: undefined initial metadata"
      }
  go st@(UpConverterState{..}) (fwdIn, bwdIn) =
    (nextState st fwdIn bwdIn, (PacketStreamS2M outReady, toPacketStream st))
   where
    outReady = not _ucFlush || _ready bwdIn

{- |
Converts packet streams of arbitrary data width @dwIn@ to packet streams of
a bigger data width @dwOut@, where @dwIn@ must divide @dwOut@. When @dwIn ~ dwOut@,
this component is set to be `idC`.

Has one cycle of latency, but full throughput.
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
  -- | Upconverter circuit
  Circuit (PacketStream dom dwIn meta) (PacketStream dom dwOut meta)
upConverterC = case sameNat (SNat @dwIn) (SNat @dwOut) of
  Just Refl -> idC
  _ -> forceResetSanity |> fromSignals upConverter

{- |
Unsafe version of 'upConverterC'.

Because 'upConverterC' runs at full throughput, i.e. it only asserts backpressure
if the subordinate asserts backpressure, we supply this variant which drops all
backpressure signals. This can be used when the source circuit does not support
backpressure. Using this variant in that case will improve timing and probably
reduce resource usage.
-}
unsafeUpConverterC ::
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
  -- | Unsafe upconverter circuit
  Circuit
    (CSignal dom (Maybe (PacketStreamM2S dwIn meta)))
    (CSignal dom (Maybe (PacketStreamM2S dwOut meta)))
unsafeUpConverterC = case sameNat (SNat @dwIn) (SNat @dwOut) of
  Just Refl -> idC
  _ -> unsafeDropBackpressure (fromSignals upConverter)

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
    (True, Just i) -> (satSub SatBound i (natToNum @dwOut), _data inPkt)
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
        ( _ready bwdIn
        , (\i -> resize $ if _dcSize == 0 then i else _dcSize)
            <$> _last inPkt
        )
    | otherwise = (False, Nothing)

  -- Keep the buffer in the state and rotate it once the byte is acknowledged to avoid
  -- dynamic indexing.
  nextSt
    | _ready bwdIn = DownConverterState newBuf nextSize
    | otherwise = st

{- | Converts packet streams of arbitrary data width @dwIn@ to packet streams of
a smaller data width @dwOut@, where @dwOut@ must divide @dwIn@. When @dwIn ~ dwOut@,
this component is set to be `idC`.

If `_abort` is asserted on an input transfer, it will be asserted on all
corresponding output transfers as well.

Provides zero latency and full throughput.
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
  -- | Downconverter circuit
  Circuit (PacketStream dom dwIn meta) (PacketStream dom dwOut meta)
downConverterC = case sameNat (SNat @dwIn) (SNat @dwOut) of
  Just Refl -> idC
  _ -> forceResetSanity |> fromSignals (mealyB downConverterT s0)
   where
    s0 =
      DownConverterState
        { _dcBuf = deepErrorX "downConverterC: undefined initial buffer"
        , _dcSize = 0
        }
