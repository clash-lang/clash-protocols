{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
Copyright  :  (C) 2024, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Provides an upconverter and downconverter for changing the data width of
packet streams.
-}
module Protocols.PacketStream.Converters (
  downConverterC,
  upConverterC,
  unsafeUpConverterC,
) where

import Clash.Prelude

import Data.Maybe (fromMaybe, isJust)
import Data.Maybe.Extra
import Data.Type.Equality ((:~:) (Refl))

import Protocols (CSignal, Circuit (..), fromSignals, idC, (|>))
import Protocols.PacketStream.Base

-- | State of 'upConverter'.
data UpConverterState (dwIn :: Nat) (n :: Nat) (meta :: Type) = UpConverterState
  { _ucBuf :: Vec (dwIn * n) (BitVector 8)
  -- ^ The data buffer we are filling.
  , _ucIdx :: Index n
  -- ^ Where in _ucBuf we need to write the next data.
  , _ucIdx2 :: Index (dwIn * n + 1)
  -- ^ Used when @dwIn@ is not a power of two to determine the adjusted '_last',
  --   to avoid multiplication (infers an expensive DSP slice).
  --   If @dwIn@ is a power of two then we can multiply by shifting left with
  --   a constant, which is free in hardware in terms of resource usage.
  , _ucFlush :: Bool
  -- ^ If true, we should output the current state as a PacketStream transfer.
  , _ucAborted :: Bool
  -- ^ Whether the current transfer we are building is aborted.
  , _ucLastIdx :: Maybe (Index (dwIn * n + 1))
  -- ^ If Just, the current buffer contains the last byte of the current packet.
  , _ucMeta :: meta
  -- ^ Metadata of the current transfer we are a building.
  }
  deriving (Generic, NFDataX, Show, ShowX)

-- | Computes the next state for 'upConverter'.
nextState ::
  forall (dwIn :: Nat) (meta :: Type) (n :: Nat).
  (1 <= dwIn) =>
  (1 <= n) =>
  (KnownNat dwIn) =>
  (KnownNat n) =>
  (NFDataX meta) =>
  UpConverterState dwIn n meta ->
  Maybe (PacketStreamM2S dwIn meta) ->
  PacketStreamS2M ->
  UpConverterState dwIn n meta
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

  nextBuf =
    bitCoerce
      $ replace
        _ucIdx
        (pack _data :: BitVector (8 * dwIn))
        (bitCoerce _ucBuf :: Vec n (BitVector (8 * dwIn)))

  nextFlush = isJust _last || bufFull
  nextIdx = if nextFlush then 0 else _ucIdx + 1

  -- If @dwIn@ is not a power of two, we need to do some extra bookkeeping to
  -- avoid multiplication to calculate _last. If not, _ucIdx2 stays at 0 and is
  -- never used, and should therefore be optimized out by synthesis tools.
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
      , _ucAborted = nextAbort
      , _ucLastIdx = nextLastIdx
      , _ucMeta = _meta
      }
  nextSt = if outReady then nextStRaw else st

upConverter ::
  forall (dwIn :: Nat) (meta :: Type) (dom :: Domain) (n :: Nat).
  (HiddenClockResetEnable dom) =>
  (1 <= dwIn) =>
  (1 <= n) =>
  (KnownNat dwIn) =>
  (KnownNat n) =>
  (NFDataX meta) =>
  ( Signal dom (Maybe (PacketStreamM2S dwIn meta))
  , Signal dom PacketStreamS2M
  ) ->
  ( Signal dom PacketStreamS2M
  , Signal dom (Maybe (PacketStreamM2S (dwIn * n) meta))
  )
upConverter = mealyB go s0
 where
  errPrefix = "upConverterT: undefined initial "
  s0 =
    UpConverterState
      { _ucBuf = repeat (nullByte "upConverter")
      , _ucIdx = 0
      , _ucIdx2 = 0
      , _ucFlush = False
      , _ucAborted = False
      , _ucLastIdx = deepErrorX (errPrefix <> " _ucLastIdx")
      , _ucMeta = deepErrorX (errPrefix <> " _ucMeta")
      }
  go st@(UpConverterState{..}) (fwdIn, bwdIn) =
    (nextState st fwdIn bwdIn, (PacketStreamS2M outReady, fwdOut))
   where
    outReady = not _ucFlush || _ready bwdIn

    fwdOut =
      toMaybe _ucFlush
        $ PacketStreamM2S
          { _data = _ucBuf
          , _last = _ucLastIdx
          , _meta = _ucMeta
          , _abort = _ucAborted
          }

{- |
Converts packet streams of arbitrary data width @dwIn@ to packet streams of
a bigger (or equal) data width @dwIn * n@, where @n > 0@.
When @n ~ 1@, this component is just the identity circuit, `idC`.

If '_abort' is asserted on any of the input sub-transfers, it will be asserted
on the corresponding output transfer as well. All zero-byte transfers are
preserved.

Has one cycle of latency, all M2S outputs are registered.
Provides full throughput.
-}
upConverterC ::
  forall (dwIn :: Nat) (n :: Nat) (meta :: Type) (dom :: Domain).
  (HiddenClockResetEnable dom) =>
  (1 <= dwIn) =>
  (1 <= n) =>
  (KnownNat dwIn) =>
  (KnownNat n) =>
  (NFDataX meta) =>
  -- | Upconverter circuit
  Circuit (PacketStream dom dwIn meta) (PacketStream dom (dwIn * n) meta)
upConverterC = case sameNat d1 (SNat @n) of
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
  forall (dwIn :: Nat) (meta :: Type) (dom :: Domain) (n :: Nat).
  (HiddenClockResetEnable dom) =>
  (1 <= dwIn) =>
  (1 <= n) =>
  (KnownNat dwIn) =>
  (KnownNat n) =>
  (NFDataX meta) =>
  -- | Unsafe upconverter circuit
  Circuit
    (CSignal dom (Maybe (PacketStreamM2S dwIn meta)))
    (CSignal dom (Maybe (PacketStreamM2S (dwIn * n) meta)))
unsafeUpConverterC = case sameNat d1 (SNat @n) of
  Just Refl -> idC
  _ -> unsafeDropBackpressure (fromSignals upConverter)

-- | State of 'downConverterT'.
data DownConverterState (dwOut :: Nat) (n :: Nat) (meta :: Type) = DownConverterState
  { _dcBuf :: Vec (dwOut * n) (BitVector 8)
  -- ^ Registered _data of the last transfer.
  , _dcLast :: Bool
  -- ^ Is the last transfer the end of a packet?
  , _dcMeta :: meta
  -- ^ Registered _meta of the last transfer.
  , _dcAborted :: Bool
  -- ^ Registered _abort of the last transfer. All sub-transfers corresponding
  --   to this transfer need to be marked with the same _abort value.
  , _dcSize :: Index (dwOut * n + 1)
  -- ^ Number of valid bytes in _dcBuf.
  , _dcZeroByteTransfer :: Bool
  -- ^ Is the current transfer we store a zero-byte transfer? In this case,
  --   _dcSize is 0 but we still need to transmit something in order to
  --   preserve zero-byte transfers.
  }
  deriving (Generic, NFDataX)

-- | State transition function of 'downConverterC', in case @n > 1@.
downConverterT ::
  forall (dwOut :: Nat) (n :: Nat) (meta :: Type).
  (KnownNat dwOut) =>
  (KnownNat n) =>
  (1 <= dwOut) =>
  (1 <= n) =>
  (NFDataX meta) =>
  DownConverterState dwOut n meta ->
  (Maybe (PacketStreamM2S (dwOut * n) meta), PacketStreamS2M) ->
  ( DownConverterState dwOut n meta
  , (PacketStreamS2M, Maybe (PacketStreamM2S dwOut meta))
  )
downConverterT st@(DownConverterState{..}) (fwdIn, bwdIn) =
  (nextSt, (PacketStreamS2M readyOut, fwdOut))
 where
  (shiftedBuf, dataOut) =
    leToPlus @dwOut @(dwOut * n)
      $ shiftOutFrom0 (SNat @dwOut) _dcBuf

  -- Either we preserve a zero-byte transfer or we have some real data to transmit.
  fwdOut =
    toMaybe (_dcSize > 0 || _dcZeroByteTransfer)
      $ PacketStreamM2S
        { _data = dataOut
        , _last =
            if _dcZeroByteTransfer
              then Just 0
              else toMaybe (_dcSize <= natToNum @dwOut && _dcLast) (resize _dcSize)
        , _meta = _dcMeta
        , _abort = _dcAborted
        }

  -- If the state buffer is empty, or if the state buffer is not empty and
  -- the final sub-transfer is acknowledged this clock cycle, we can acknowledge
  -- newly received valid data and load it into our registers.
  emptyState = _dcSize == 0 && not _dcZeroByteTransfer
  readyOut =
    isJust fwdIn
      && (emptyState || (_dcSize <= natToNum @dwOut && _ready bwdIn))

  nextSt
    | readyOut = newState (fromJustX fwdIn)
    | not emptyState && _ready bwdIn =
        st
          { _dcBuf = shiftedBuf
          , _dcSize = satSub SatBound _dcSize (natToNum @dwOut)
          , _dcZeroByteTransfer = False
          }
    | otherwise = st

  -- Computes a new state from a valid incoming transfer.
  newState PacketStreamM2S{..} =
    DownConverterState
      { _dcBuf = _data
      , _dcMeta = _meta
      , _dcSize = fromMaybe (natToNum @(dwOut * n)) _last
      , _dcLast = isJust _last
      , _dcAborted = _abort
      , _dcZeroByteTransfer = _last == Just 0
      }

{- |
Converts packet streams of a data width which is a multiple of @n@, i.e.
@dwOut * n@, to packet streams of a smaller (or equal) data width @dwOut@,
where @n > 0@.
When @n ~ 1@, this component is just the identity circuit, `idC`.

If '_abort' is asserted on an input transfer, it will be asserted on all
corresponding output sub-transfers as well. All zero-byte transfers are
preserved.

Has one clock cycle of latency, all M2S outputs are registered.
Throughput is optimal, a transfer of @k@ valid bytes is transmitted in @k@
clock cycles. To be precise, throughput is at least @(1 / n)%@, so at
least @50%@ if @n = 2@ for example. We specify /at least/,
because the throughput may be on the last transfer of a packet, when not all
bytes have to be valid. If there is only one valid byte in the last transfer,
then the throughput will always be @100%@ for that particular transfer.
-}
downConverterC ::
  forall (dwOut :: Nat) (n :: Nat) (meta :: Type) (dom :: Domain).
  (HiddenClockResetEnable dom) =>
  (KnownNat dwOut) =>
  (KnownNat n) =>
  (1 <= dwOut) =>
  (1 <= n) =>
  (NFDataX meta) =>
  -- | Downconverter circuit
  Circuit (PacketStream dom (dwOut * n) meta) (PacketStream dom dwOut meta)
downConverterC = case sameNat d1 (SNat @n) of
  Just Refl -> idC
  _ -> forceResetSanity |> fromSignals (mealyB downConverterT s0)
   where
    errPrefix = "downConverterT: undefined initial "
    s0 =
      DownConverterState
        { _dcBuf = deepErrorX (errPrefix <> "_dcBuf")
        , _dcLast = deepErrorX (errPrefix <> "_dcLast")
        , _dcMeta = deepErrorX (errPrefix <> "_dcMeta")
        , _dcAborted = deepErrorX (errPrefix <> "_dcAborted")
        , _dcSize = 0
        , _dcZeroByteTransfer = False
        }
