{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
Copyright  :  (C) 2024, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Provides `asyncFifoC` for crossing clock domains in the packet stream protocol.
-}
module Protocols.PacketStream.AsyncFifo (asyncFifoC) where

import Data.Maybe.Extra (toMaybe)

import Clash.Explicit.Prelude (asyncFIFOSynchronizer)
import Clash.Prelude

import Protocols
import Protocols.PacketStream.Base

{- | Asynchronous FIFO circuit that can be used to safely cross clock domains.
Uses `Clash.Explicit.Prelude.asyncFIFOSynchronizer` internally.
-}
asyncFifoC ::
  forall
    (wDom :: Domain)
    (rDom :: Domain)
    (depth :: Nat)
    (dataWidth :: Nat)
    (meta :: Type).
  (KnownDomain wDom) =>
  (KnownDomain rDom) =>
  (KnownNat depth) =>
  (KnownNat dataWidth) =>
  (2 <= depth) =>
  (1 <= dataWidth) =>
  (NFDataX meta) =>
  -- | 2^depth is the number of elements this component can store
  SNat depth ->
  -- | Clock signal in the write domain
  Clock wDom ->
  -- | Reset signal in the write domain
  Reset wDom ->
  -- | Enable signal in the write domain
  Enable wDom ->
  -- | Clock signal in the read domain
  Clock rDom ->
  -- | Reset signal in the read domain
  Reset rDom ->
  -- | Enable signal in the read domain
  Enable rDom ->
  Circuit (PacketStream wDom dataWidth meta) (PacketStream rDom dataWidth meta)
asyncFifoC depth wClk wRst wEn rClk rRst rEn =
  exposeClockResetEnable forceResetSanity wClk wRst wEn |> fromSignals ckt
 where
  ckt (fwdIn, bwdIn) = (bwdOut, fwdOut)
   where
    (element, isEmpty, isFull) = asyncFIFOSynchronizer depth wClk rClk wRst rRst wEn rEn readReq fwdIn
    notEmpty = not <$> isEmpty
    -- If the FIFO is empty, we output Nothing. Else, we output the oldest element.
    fwdOut = toMaybe <$> notEmpty <*> element
    -- Assert backpressure when the FIFO is full.
    bwdOut = PacketStreamS2M . not <$> isFull
    -- Next component is ready to read if the fifo is not empty and it does not assert backpressure.
    readReq = notEmpty .&&. _ready <$> bwdIn
