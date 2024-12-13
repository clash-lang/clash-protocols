{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
Copyright  :  (C) 2024, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Optimized store and forward FIFO circuit for packet streams.
-}
module Protocols.PacketStream.PacketFifo (
  packetFifoC,
  FullMode (..),
) where

import Clash.Prelude

import Data.Maybe
import Data.Maybe.Extra (toMaybe)

import Protocols
import Protocols.PacketStream.Base

type PacketStreamContent (dataWidth :: Nat) (meta :: Type) =
  (Vec dataWidth (BitVector 8), Maybe (Index (dataWidth + 1)))

-- | Specifies the behaviour of `packetFifoC` when it is full.
data FullMode
  = -- | Assert backpressure when the FIFO is full.
    Backpressure
  | -- | Drop new packets when the FIFO is full.
    --   The FIFO never asserts backpressure.
    Drop

toPacketStreamContent ::
  PacketStreamM2S dataWidth meta -> PacketStreamContent dataWidth meta
toPacketStreamContent PacketStreamM2S{..} = (_data, _last)

toPacketStreamM2S ::
  PacketStreamContent dataWidth meta -> meta -> PacketStreamM2S dataWidth meta
toPacketStreamM2S (d, l) m = PacketStreamM2S d l m False

data PacketFifoState contentDepth metaDepth = PacketFifoState
  { _canRead :: Bool
  -- ^ We need this to avoid read-write conflicts.
  , _dropping :: Bool
  -- ^ Whether we are dropping the current packet.
  , _basePtr :: Unsigned contentDepth
  -- ^ Points to the base address of the current packet, i.e. the address of
  --   the first transfer.
  , _cReadPtr :: Unsigned contentDepth
  -- ^ Read pointer in the content block ram.
  , _cWritePtr :: Unsigned contentDepth
  -- ^ Write pointer in the content block ram.
  , _mReadPtr :: Unsigned metaDepth
  -- ^ Read pointer in the metadata block ram.
  , _mWritePtr :: Unsigned metaDepth
  -- ^ Write pointer in the metadata block ram.
  }
  deriving (Generic, NFDataX, Show, ShowX)

-- | State transition function of 'packetFifoC', mode @Backpressure@.
packetFifoT ::
  forall
    (dataWidth :: Nat)
    (meta :: Type)
    (contentDepth :: Nat)
    (metaDepth :: Nat).
  (KnownNat dataWidth) =>
  (KnownNat contentDepth) =>
  (KnownNat metaDepth) =>
  (1 <= contentDepth) =>
  (1 <= metaDepth) =>
  (NFDataX meta) =>
  PacketFifoState contentDepth metaDepth ->
  ( Maybe (PacketStreamM2S dataWidth meta)
  , PacketStreamS2M
  , PacketStreamContent dataWidth meta
  , meta
  ) ->
  ( PacketFifoState contentDepth metaDepth
  , ( Unsigned contentDepth
    , Unsigned metaDepth
    , Maybe (Unsigned contentDepth, PacketStreamContent dataWidth meta)
    , Maybe (Unsigned metaDepth, meta)
    , PacketStreamS2M
    , Maybe (PacketStreamM2S dataWidth meta)
    )
  )
packetFifoT st@PacketFifoState{..} (fwdIn, bwdIn, cRam, mRam) =
  (nextSt, (cReadPtr', mReadPtr', cWriteCmd, mWriteCmd, bwdOut, fwdOut))
 where
  -- Status signals
  pktTooBig = _cWritePtr + 1 == _cReadPtr && fifoEmpty
  (lastPkt, dropping) = case fwdIn of
    Nothing -> (False, _dropping || pktTooBig)
    Just PacketStreamM2S{..} -> (isJust _last, _dropping || pktTooBig || _abort)

  fifoEmpty = _mReadPtr == _mWritePtr
  fifoSinglePacket = _mReadPtr + 1 == _mWritePtr
  fifoFull =
    (_cWritePtr + 1 == _cReadPtr)
      || (_mWritePtr + 1 == _mReadPtr && lastPkt)

  -- Enables
  readEn = _canRead && not fifoEmpty
  cReadEn = readEn && _ready bwdIn
  mReadEn = readEn && _ready bwdIn && isJust (snd cRam)

  -- Output
  bwdOut = PacketStreamS2M (not fifoFull || dropping)
  fwdOut =
    if readEn
      then Just (toPacketStreamM2S cRam mRam)
      else Nothing

  -- New state

  -- Our block RAM is read-before-write, so we cannot use the read value next
  -- clock cycle if there is a read-write conflict. Such a conflict might happen
  -- when we finish writing a packet into the FIFO while:
  -- 1. The FIFO is empty.
  -- 2. The FIFO has one packet inside and we finish outputting it this clock cycle.
  canRead' = not (lastPkt && (fifoEmpty || (mReadEn && fifoSinglePacket)))
  dropping' = dropping && not lastPkt

  basePtr' = if lastPkt && _ready bwdOut then cWritePtr' else _basePtr
  cReadPtr' = if cReadEn then _cReadPtr + 1 else _cReadPtr
  mReadPtr' = if mReadEn then _mReadPtr + 1 else _mReadPtr

  (cWriteCmd, cWritePtr') =
    if not dropping && not fifoFull
      then
        ( (\t -> (_cWritePtr, toPacketStreamContent t)) <$> fwdIn
        , _cWritePtr + 1
        )
      else
        ( Nothing
        , if dropping then _basePtr else _cWritePtr
        )

  -- Write the metadata into RAM upon the last transfer of a packet, and
  -- advance the write pointer. This allows us to write the data of a packet
  -- into RAM even if the metadata RAM is currently full (hoping that it will
  -- free up before we read the end of the packet). It also prevents unnecessary
  -- writes in case a packet is aborted or too big.
  (mWriteCmd, mWritePtr') =
    if not dropping && not fifoFull && lastPkt
      then ((\t -> (_mWritePtr, _meta t)) <$> fwdIn, _mWritePtr + 1)
      else (Nothing, _mWritePtr)

  nextSt = case fwdIn of
    Nothing -> st{_canRead = True, _cReadPtr = cReadPtr', _mReadPtr = mReadPtr'}
    Just _ ->
      PacketFifoState
        { _canRead = canRead'
        , _dropping = dropping'
        , _basePtr = basePtr'
        , _cReadPtr = cReadPtr'
        , _cWritePtr = cWritePtr'
        , _mReadPtr = mReadPtr'
        , _mWritePtr = mWritePtr'
        }

-- | Implementation of 'packetFifoC', mode @Drop@.
packetFifoImpl ::
  forall
    (dom :: Domain)
    (dataWidth :: Nat)
    (meta :: Type)
    (contentSizeBits :: Nat)
    (metaSizeBits :: Nat).
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (1 <= contentSizeBits) =>
  (1 <= metaSizeBits) =>
  (NFDataX meta) =>
  -- | Depth of the content of the packet buffer, this is equal to 2^contentSizeBits
  SNat contentSizeBits ->
  -- | Depth of the content of the meta buffer, this is equal to 2^metaSizeBits
  SNat metaSizeBits ->
  -- | Input packetStream
  ( Signal dom (Maybe (PacketStreamM2S dataWidth meta))
  , Signal dom PacketStreamS2M
  ) ->
  -- | Output CSignal s
  ( Signal dom PacketStreamS2M
  , Signal dom (Maybe (PacketStreamM2S dataWidth meta))
  )
packetFifoImpl SNat SNat (fwdIn, bwdIn) = (PacketStreamS2M . not <$> fullBuffer, fwdOut)
 where
  fwdOut = toMaybe <$> (not <$> emptyBuffer) <*> (toPacketStreamM2S <$> ramContent <*> ramMeta)

  -- The backing ram
  ramContent =
    blockRam1
      NoClearOnReset
      (SNat @(2 ^ contentSizeBits))
      (errorX "initial block ram content")
      cReadAddr'
      writeCommand
  ramMeta =
    blockRam1
      NoClearOnReset
      (SNat @(2 ^ metaSizeBits))
      (errorX "initial block ram meta content")
      mReadAddr'
      mWriteCommand

  -- The write commands to the RAM
  writeCommand =
    toMaybe
      <$> writeEnable
      <*> bundle (cWordAddr, toPacketStreamContent . fromJustX <$> fwdIn)
  mWriteCommand = toMaybe <$> nextPacketIn <*> bundle (mWriteAddr, _meta . fromJustX <$> fwdIn)

  -- Addresses for the content data
  cWordAddr, cPacketAddr, cReadAddr :: Signal dom (Unsigned contentSizeBits)
  cWordAddr = register 0 $ mux dropping' cPacketAddr $ mux writeEnable (cWordAddr + 1) cWordAddr
  cPacketAddr = register 0 $ mux nextPacketIn (cWordAddr + 1) cPacketAddr
  cReadAddr' = mux readEnable (cReadAddr + 1) cReadAddr
  cReadAddr = register 0 cReadAddr'

  -- Addresses for the meta data
  mWriteAddr, mReadAddr :: Signal dom (Unsigned metaSizeBits)
  mWriteAddr = register 0 mWriteAddr'
  mWriteAddr' = mux nextPacketIn (mWriteAddr + 1) mWriteAddr

  mReadAddr' = mux mReadEnable (mReadAddr + 1) mReadAddr
  mReadAddr = register 0 mReadAddr'
  -- only read the next value if we've outputted the last word of a packet
  mReadEnable = lastWordOut .&&. readEnable

  -- Registers : status
  dropping', dropping, emptyBuffer :: Signal dom Bool
  -- start dropping packet on abort
  dropping' = abortIn .||. dropping
  dropping = register False $ dropping' .&&. (not <$> lastWordIn)
  -- the buffer is empty if the metaBuffer is empty as the meta buffer
  -- only updates when a packet is complete
  emptyBuffer = register 0 mWriteAddr .==. mReadAddr

  -- Only write if there is space and we're not dropping
  writeEnable = writeRequest .&&. (not <$> fullBuffer) .&&. (not <$> dropping')
  -- Read when the word has been received
  readEnable = (not <$> emptyBuffer) .&&. (_ready <$> bwdIn)

  -- The status signals
  fullBuffer = ((cWordAddr + 1) .==. cReadAddr) .||. ((mWriteAddr + 1) .==. mReadAddr)
  writeRequest = isJust <$> fwdIn
  lastWordIn = maybe False (isJust . _last) <$> fwdIn
  lastWordOut = maybe False (isJust . _last) <$> fwdOut
  abortIn = maybe False _abort <$> fwdIn
  nextPacketIn = lastWordIn .&&. writeEnable

{- |
FIFO circuit optimized for the PacketStream protocol. Contains two FIFOs, one
for packet data ('_data', '_last') and one for packet metadata ('_meta').
Because metadata is constant per packet, the metadata FIFO can be significantly
shallower, saving resources.

Moreover, the output of the FIFO has some other properties:

- All packets which contain a transfer with '_abort' set are dropped.
- All packets that are bigger than or equal to @2^contentDepth - 1@ transfers are dropped.
- There are no gaps in output packets, i.e. @Nothing@ in between valid transfers of a packet.

The circuit is able to satisfy these properties because it first loads an entire
packet before it may transmit it. That is also why packets bigger than the
content FIFO need to be dropped.

Two modes can be selected:

- @Backpressure@: assert backpressure like normal when the FIFO is full.
- @Drop@: never give backpressure, instead drop the current packet we are loading.
-}
packetFifoC ::
  forall
    (dom :: Domain)
    (dataWidth :: Nat)
    (meta :: Type)
    (contentDepth :: Nat)
    (metaDepth :: Nat).
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (1 <= contentDepth) =>
  (1 <= metaDepth) =>
  (NFDataX meta) =>
  -- |  The content FIFO will contain @2^contentDepth@ entries.
  SNat contentDepth ->
  -- | The metadata FIFO will contain @2^metaDepth@ entries.
  SNat metaDepth ->
  -- | The backpressure behaviour of the FIFO when it is full.
  FullMode ->
  Circuit (PacketStream dom dataWidth meta) (PacketStream dom dataWidth meta)
packetFifoC cSize@SNat mSize@SNat fullMode =
  let
    ckt (fwdIn, bwdIn) = (bwdOut, fwdOut)
     where
      ramContent =
        blockRam1
          NoClearOnReset
          (SNat @(2 ^ contentDepth))
          (deepErrorX "initial block ram content")
          cReadPtr
          cWriteCommand
      ramMeta =
        blockRam1
          NoClearOnReset
          (SNat @(2 ^ metaDepth))
          (deepErrorX "initial block ram meta content")
          mReadPtr
          mWriteCommand

      (cReadPtr, mReadPtr, cWriteCommand, mWriteCommand, bwdOut, fwdOut) =
        mealyB
          (packetFifoT @dataWidth @meta @contentDepth @metaDepth)
          (PacketFifoState False False 0 0 0 0 0)
          (fwdIn, bwdIn, ramContent, ramMeta)
   in
    case fullMode of
      Backpressure ->
        forceResetSanity |> fromSignals ckt
      Drop ->
        toCSignal
          |> unsafeAbortOnBackpressureC
          |> forceResetSanity
          |> fromSignals (packetFifoImpl cSize mSize)
