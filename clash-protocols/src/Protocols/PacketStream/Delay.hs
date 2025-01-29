{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
Copyright  :  (C) 2024, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Provides a circuit that delays a stream by a configurable amount of transfers.
-}
module Protocols.PacketStream.Delay (
  delayStreamC,
) where

import Clash.Prelude

import Protocols
import Protocols.PacketStream.Base

import Data.Constraint.Deferrable ((:~:) (Refl))
import Data.Maybe
import GHC.Stack (HasCallStack)

type M2SNoMeta dataWidth =
  (Vec dataWidth (BitVector 8), Maybe (Index (dataWidth + 1)), Bool)

toPacketstreamM2S :: M2SNoMeta dataWidth -> meta -> PacketStreamM2S dataWidth meta
toPacketstreamM2S (a, b, c) d = PacketStreamM2S a b d c

dropMeta :: PacketStreamM2S dataWidth meta -> M2SNoMeta dataWidth
dropMeta PacketStreamM2S{..} = (_data, _last, _abort)

-- | State of 'delayStreamT'.
data DelayState n = DelayState
  { _size :: Index (n + 1)
  -- ^ The number of valid transactions in the buffer.
  , _readPtr :: Index n
  -- ^ Current block RAM read address.
  , _writePtr :: Index n
  -- ^ Current block RAM write address.
  , _flush :: Bool
  -- ^ Iff true, transmit all remaining transactions of the current packet,
  --   regardless of whether the buffer is full or not.
  , _metaWriteEn :: Bool
  -- ^ If true, write to the meta buffer. We need this in case of LHS stalls.
  }
  deriving (Generic, NFDataX, Show, ShowX)

-- | State transition function of 'delayStreamC'.
delayStreamT ::
  forall
    (n :: Nat)
    (dataWidth :: Nat)
    (meta :: Type).
  (KnownNat n) =>
  (KnownNat dataWidth) =>
  (1 <= n) =>
  (1 <= dataWidth) =>
  (NFDataX meta) =>
  DelayState n ->
  ( Maybe (PacketStreamM2S dataWidth meta)
  , PacketStreamS2M
  , M2SNoMeta dataWidth
  , meta
  ) ->
  ( DelayState n
  , ( PacketStreamS2M
    , Maybe (PacketStreamM2S dataWidth meta)
    , Index n
    , Maybe (Index n, M2SNoMeta dataWidth)
    , Maybe meta
    )
  )
delayStreamT st (fwdIn, bwdIn, buff@(_, b, _), metaBuf) =
  (nextStOut, (bwdOut, fwdOut, _readPtr nextStOut, writeCmd, mWriteCmd))
 where
  emptyBuf = _size st == 0
  fullBuf = _size st == maxBound

  readEn = case fwdIn of
    Nothing -> not emptyBuf && _flush st
    Just _ -> fullBuf || _flush st

  bwdOut = PacketStreamS2M (isNothing fwdIn || not readEn || _ready bwdIn)

  (readPtr', fwdOut) =
    if readEn
      then (satSucc SatWrap (_readPtr st), Just (toPacketstreamM2S buff metaBuf))
      else (_readPtr st, Nothing)

  (writeCmd, mWriteCmd, writePtr') = case fwdIn of
    Nothing -> (Nothing, Nothing, _writePtr st)
    Just inPkt ->
      ( if readEn && not (_ready bwdIn)
          then Nothing
          else Just (_writePtr st, dropMeta inPkt)
      , if _metaWriteEn st || (emptyBuf || readEn && isJust b && _ready bwdIn)
          then Just (_meta inPkt)
          else Nothing
      , satSucc SatWrap (_writePtr st)
      )

  metaWriteEn' = isNothing fwdIn && (_metaWriteEn st || isJust b)

  (size', flush') = case fwdIn of
    Nothing -> (_size st - 1, isNothing b && _flush st)
    Just inPkt
      | _flush st ->
          (_size st, not (readEn && isJust b) && _flush st)
      | otherwise ->
          (satSucc SatBound (_size st), isJust (_last inPkt))

  nextSt = DelayState size' readPtr' writePtr' flush' metaWriteEn'
  nextStOut =
    if isJust fwdIn
      && (isNothing fwdOut || _ready bwdIn)
      || isNothing fwdIn
      && (readEn && _ready bwdIn)
      then nextSt
      else st

blockRamUNoClear ::
  forall n dom a addr.
  ( HasCallStack
  , HiddenClockResetEnable dom
  , NFDataX a
  , Enum addr
  , NFDataX addr
  , 1 <= n ) =>
  SNat n ->
  Signal dom addr ->
  Signal dom (Maybe (addr, a)) ->
  Signal dom a
#if MIN_VERSION_clash_prelude(1,9,0)
blockRamUNoClear = blockRamU NoClearOnReset
#else
blockRamUNoClear n =
  blockRamU NoClearOnReset n (errorX "No reset function")
#endif

{- |
Forwards incoming packets with @n@ transactions latency. Because of potential
stalls this is not the same as @n@ clock cycles. Assumes that all packets
passing through this component are bigger than @n@ transactions. If not, this
component has __UNDEFINED BEHAVIOUR__ and things will break.
-}
delayStreamC ::
  forall
    (dataWidth :: Nat)
    (meta :: Type)
    (dom :: Domain)
    (n :: Nat).
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (1 <= n) =>
  (1 <= dataWidth) =>
  (NFDataX meta) =>
  -- | The number of transactions to delay
  SNat n ->
  Circuit (PacketStream dom dataWidth meta) (PacketStream dom dataWidth meta)
delayStreamC SNat = forceResetSanity |> fromSignals ckt
 where
  ckt (fwdInS, bwdInS) = (bwdOutS, fwdOutS)
   where
    -- Store the contents of transactions without metadata.
    -- We only need write before read semantics in case n ~ 1.
    bram :: Signal dom (M2SNoMeta dataWidth)
    bram = case sameNat d1 (SNat @n) of
      Nothing -> blockRamUNoClear (SNat @n) readAddr writeCmd
      Just Refl -> readNew (blockRamUNoClear (SNat @n)) readAddr writeCmd

    -- There are at most two packets in the blockram, but they are required
    -- to be bigger than @n@ transactions. Thus, we only need to store the
    -- metadata once.
    metaBuffer :: Signal dom meta
    metaBuffer = regMaybe (deepErrorX "delayStream: undefined initial meta") mWriteCmd

    (bwdOutS, fwdOutS, readAddr, writeCmd, mWriteCmd) =
      unbundle
        $ mealy delayStreamT (DelayState @n 0 0 0 False True) input

    input = bundle (fwdInS, bwdInS, bram, metaBuffer)
