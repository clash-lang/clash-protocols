{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
Optimized Store and forward FIFO circuit for packet streams.
-}
module Protocols.PacketStream.PacketFifo (
  packetFifoC,
  FullMode (..),
) where

import Clash.Prelude

import Protocols
import Protocols.PacketStream.Base

import Data.Maybe
import Data.Maybe.Extra

type PacketStreamContent (dataWidth :: Nat) (meta :: Type) =
  (Vec dataWidth (BitVector 8), Maybe (Index dataWidth))

toPacketStreamContent ::
  PacketStreamM2S dataWidth meta -> PacketStreamContent dataWidth meta
toPacketStreamContent PacketStreamM2S{_data = d, _last = l, _meta = _, _abort = _} = (d, l)

toPacketStreamM2S ::
  PacketStreamContent dataWidth meta -> meta -> PacketStreamM2S dataWidth meta
toPacketStreamM2S (d, l) m = PacketStreamM2S d l m False

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
  -- only read the next value if we've outpustted the last word of a packet
  mReadEnable = lastWordOut .&&. readEnable

  -- Registers : status
  dropping', dropping, emptyBuffer :: Signal dom Bool
  -- start dropping packet on abort
  dropping' = abortIn .||. dropping
  dropping = register False $ dropping' .&&. (not <$> lastWordIn)
  -- the buffer is empty if the metaBuffer is empty as the metabuffer only updates when a packet is complete
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
Packet buffer, a circuit which stores words in a buffer until the packet is complete.
Once a packet is complete it will send the entire packet out at once without stalls.
If a transfer in a packet has `_abort` set to true, the packetBuffer will drop the entire packet.

__UNSAFE__: if `FullMode` is set to @Backpressure@ and a packet containing
@>= 2^contentSizeBits-1@ transfers is loaded into the FIFO, it will deadlock.
-}
packetFifoC ::
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
  -- | The FIFO can store @2^contentSizeBits@ transfers
  SNat contentSizeBits ->
  -- | The FIFO can store @2^metaSizeBits@ packets
  SNat metaSizeBits ->
  -- | Specifies the behaviour of the FIFO when it is full
  FullMode ->
  Circuit (PacketStream dom dataWidth meta) (PacketStream dom dataWidth meta)
packetFifoC cSizeBits mSizeBits mode = case mode of
  Backpressure ->
    forceResetSanity
      |> fromSignals (packetFifoImpl cSizeBits mSizeBits)
  Drop ->
    fromPacketStream
      |> abortOnBackPressureC
      |> forceResetSanity
      |> fromSignals (packetFifoImpl cSizeBits mSizeBits)

-- | Specifies the behaviour of `packetFifoC` when it is full.
data FullMode
  = -- | Assert backpressure when the FIFO is full.
    Backpressure
  | -- | Drop new packets when the FIFO is full.
    --   The FIFO never asserts backpressure.
    Drop
