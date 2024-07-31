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

import Protocols (Circuit (..), fromSignals, (|>))
import Protocols.PacketStream.Base

import Data.Maybe (isJust, isNothing)
import Data.Maybe.Extra

-- | Upconverter state, consisting of at most p `BitVector 8`s and a vector indicating which bytes are valid
data UpConverterState (dataWidth :: Nat) = UpConverterState
  { _ucBuf :: Vec dataWidth (BitVector 8)
  -- ^ The buffer we are filling
  , _ucIdx :: Index dataWidth
  -- ^ Where in the buffer we need to write the next element
  , _ucFlush :: Bool
  -- ^ If this is true the current state can presented as packetstream word
  , _ucFreshBuf :: Bool
  -- ^ If this is true we need to start a fresh buffer
  , _ucAborted :: Bool
  -- ^ Current packet is aborted
  , _ucLastIdx :: Maybe (Index dataWidth)
  -- ^ If true the current buffer contains the last byte of the current packet
  }
  deriving (Generic, NFDataX)

toPacketStream :: UpConverterState dataWidth -> Maybe (PacketStreamM2S dataWidth ())
toPacketStream UpConverterState{..} = toMaybe _ucFlush (PacketStreamM2S _ucBuf _ucLastIdx () _ucAborted)

nextState ::
  (KnownNat dataWidth) =>
  UpConverterState dataWidth ->
  Maybe (PacketStreamM2S 1 ()) ->
  PacketStreamS2M ->
  UpConverterState dataWidth
nextState st@(UpConverterState{..}) Nothing (PacketStreamS2M inReady) =
  nextSt
 where
  outReady = not _ucFlush || inReady
  -- If we can accept data we can always set _ucFlush to false,
  -- since we only change state if we can transmit and receive data
  nextStRaw =
    st
      { _ucFlush = False
      , _ucAborted = isNothing _ucLastIdx && _ucAborted
      , _ucLastIdx = Nothing
      }
  nextSt = if outReady then nextStRaw else st
nextState st@(UpConverterState{..}) (Just PacketStreamM2S{..}) (PacketStreamS2M inReady) =
  nextSt
 where
  inLast = isJust _last
  -- We smear an abort over the entire rest of the packet
  -- so the next abort is set:
  --  - If fragment we are potentially flushing was not the last and we were already aborting;
  --  - or if the incoming fragment is aborted
  nextAbort = (isNothing _ucLastIdx && _ucAborted) || _abort
  -- If we are not flushing we can accept data to be stored in _ucBuf,
  -- but when we are flushing we can only accept if the current
  -- output fragment is accepted by the sink
  outReady = not _ucFlush || inReady
  bufFull = _ucIdx == maxBound
  currBuf = if _ucFreshBuf then (repeat 0) else _ucBuf
  nextBuf = replace _ucIdx (head _data) currBuf

  nextFlush = inLast || bufFull
  nextIdx = if nextFlush then 0 else _ucIdx + 1

  nextStRaw =
    UpConverterState
      { _ucBuf = nextBuf
      , _ucIdx = nextIdx
      , _ucFlush = nextFlush
      , _ucFreshBuf = nextFlush
      , _ucAborted = nextAbort
      , _ucLastIdx = toMaybe inLast _ucIdx
      }
  nextSt = if outReady then nextStRaw else st

upConverter ::
  forall (dataWidth :: Nat) (dom :: Domain).
  (HiddenClockResetEnable dom) =>
  (1 <= dataWidth) =>
  (KnownNat dataWidth) =>
  -- | Input packet stream from the source
  --   Input backpressure from the sink
  ( Signal dom (Maybe (PacketStreamM2S 1 ()))
  , Signal dom PacketStreamS2M
  ) ->
  -- | Output backpressure to the source
  --   Output packet stream to the sink
  ( Signal dom PacketStreamS2M
  , Signal dom (Maybe (PacketStreamM2S dataWidth ()))
  )
upConverter = mealyB go s0
 where
  s0 = UpConverterState (repeat undefined) 0 False True False Nothing
  go ::
    UpConverterState dataWidth ->
    (Maybe (PacketStreamM2S 1 ()), PacketStreamS2M) ->
    ( UpConverterState dataWidth
    , (PacketStreamS2M, Maybe (PacketStreamM2S dataWidth ()))
    )
  go st@(UpConverterState{..}) (fwdIn, bwdIn) =
    (nextState st fwdIn bwdIn, (PacketStreamS2M outReady, toPacketStream st))
   where
    outReady = not _ucFlush || (_ready bwdIn)

{- | Converts packet streams of single bytes to packet streams of a higher data widths.
Has one cycle of latency, but optimal throughput.
-}
upConverterC ::
  forall (dataWidth :: Nat) (dom :: Domain).
  (HiddenClockResetEnable dom) =>
  (1 <= dataWidth) =>
  (KnownNat dataWidth) =>
  Circuit (PacketStream dom 1 ()) (PacketStream dom dataWidth ())
upConverterC = forceResetSanity |> fromSignals upConverter

data DownConverterState (dataWidth :: Nat) = DownConverterState
  { _dcBuf :: Vec dataWidth (BitVector 8)
  -- ^ Buffer
  , _dcSize :: Index (dataWidth + 1)
  -- ^ Number of valid bytes in _dcBuf
  }
  deriving (Generic, NFDataX)

downConverterT ::
  forall (dataWidth :: Nat).
  (1 <= dataWidth) =>
  (KnownNat dataWidth) =>
  DownConverterState dataWidth ->
  (Maybe (PacketStreamM2S dataWidth ()), PacketStreamS2M) ->
  (DownConverterState dataWidth, (PacketStreamS2M, Maybe (PacketStreamM2S 1 ())))
downConverterT st (Nothing, _) = (st, (PacketStreamS2M True, Nothing))
downConverterT st@DownConverterState{..} (Just inPkt, bwdIn) = (nextSt, (PacketStreamS2M outReady, Just outPkt))
 where
  -- If _dcSize == 0, then we have received a new transfer and should use
  -- its corresponding _data. Else, we should use our stored buffer.
  (nextSize, buf) = case (_dcSize == 0, _last inPkt) of
    (True, Nothing) -> (natToNum @(dataWidth - 1), _data inPkt)
    (True, Just i) -> (resize i, _data inPkt)
    (False, _) -> (pred _dcSize, _dcBuf)

  outPkt =
    PacketStreamM2S
      { _data = singleton (leToPlus @1 @dataWidth head buf)
      , _last = outLast
      , _meta = ()
      , _abort = _abort inPkt
      }

  (outReady, outLast)
    | nextSize == 0 = (_ready bwdIn, 0 <$ _last inPkt)
    | otherwise = (False, Nothing)

  -- Keep the buffer in the state and rotate it once the byte is acknowledged to avoid
  -- dynamic indexing.
  nextSt
    | _ready bwdIn = DownConverterState (rotateLeftS buf d1) nextSize
    | otherwise = st

{- | Converts packet streams of arbitrary data widths to packet streams of single bytes.
If @_abort@ is asserted on an input transfer, it will be asserted on all
corresponding output transfers as well.

Has one clock cycle of latency, but optimal throughput, i.e. a packet of n bytes is
sent out in n clock cycles, even if `_last` is set.
-}
downConverterC ::
  forall (dataWidth :: Nat) (dom :: Domain).
  (HiddenClockResetEnable dom) =>
  (1 <= dataWidth) =>
  (KnownNat dataWidth) =>
  Circuit (PacketStream dom dataWidth ()) (PacketStream dom 1 ())
downConverterC = forceResetSanity |> fromSignals (mealyB downConverterT s0)
 where
  s0 =
    DownConverterState
      { _dcBuf = errorX "downConverter: undefined initial value"
      , _dcSize = 0
      }
