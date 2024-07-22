{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
Utility circuits for appending headers to the beginning of packets.
-}
module Protocols.PacketStream.Packetizers (
  packetizerC,
  packetizeFromDfC,
) where

import Clash.Prelude

import Protocols
import qualified Protocols.Df as Df
import Protocols.PacketStream.Base

import Data.Maybe
import Data.Maybe.Extra
import Data.Type.Equality ((:~:) (Refl))

defaultByte :: BitVector 8
defaultByte = 0x00

type HeaderBufSize (headerBytes :: Nat) (dataWidth :: Nat) =
  headerBytes + dataWidth

-- The amount of bytes that we still need to forward due to
-- @headerBytes@ not aligning with @dataWidth@.
type ForwardBufSize (headerBytes :: Nat) (dataWidth :: Nat) =
  headerBytes `Mod` dataWidth

type PacketizerCt (headerBytes :: Nat) (dataWidth :: Nat) =
  ( KnownNat dataWidth
  , 1 <= dataWidth
  , KnownNat headerBytes
  )

data PacketizerState (metaOut :: Type) (headerBytes :: Nat) (dataWidth :: Nat)
  = Insert
      { _counter :: Index (headerBytes `DivRU` dataWidth)
      , _hdrBuf :: Vec (HeaderBufSize headerBytes dataWidth) (BitVector 8)
      , _aborted :: Bool
      }
  | Forward
      { _fwdBuf :: Vec (ForwardBufSize headerBytes dataWidth) (BitVector 8)
      , _aborted :: Bool
      }
  | LastForward
      {_lastFragment :: PacketStreamM2S dataWidth metaOut}
  deriving (Generic, Show, ShowX)

deriving instance
  (NFDataX metaOut, PacketizerCt headerBytes dataWidth) =>
  NFDataX (PacketizerState metaOut headerBytes dataWidth)

-- | Initial state of @packetizerT@
instance
  (PacketizerCt headerBytes dataWidth) =>
  Default (PacketizerState metaOut headerBytes dataWidth)
  where
  def :: PacketizerState metaOut headerBytes dataWidth
  def = Insert 0 (repeat defaultByte) False

adjustLast ::
  forall
    (headerBytes :: Nat)
    (dataWidth :: Nat).
  ( headerBytes `Mod` dataWidth <= dataWidth
  , KnownNat dataWidth
  , 1 <= dataWidth
  ) =>
  SNat headerBytes ->
  Index dataWidth ->
  Either (Index dataWidth) (Index dataWidth)
adjustLast SNat idx = if outputNow then Left nowIdx else Right nextIdx
 where
  outputNow = case compareSNat (SNat @(ForwardBufSize headerBytes dataWidth)) d0 of
    SNatGT -> idx < natToNum @(dataWidth - ForwardBufSize headerBytes dataWidth)
    _ -> True
  nowIdx = idx + natToNum @(ForwardBufSize headerBytes dataWidth)
  nextIdx = idx - natToNum @(dataWidth - ForwardBufSize headerBytes dataWidth)

packetizerT ::
  forall
    (headerBytes :: Nat)
    (dataWidth :: Nat)
    (header :: Type)
    (metaIn :: Type)
    (metaOut :: Type).
  (BitSize header ~ headerBytes * 8) =>
  (BitPack header) =>
  (PacketizerCt headerBytes dataWidth) =>
  (ForwardBufSize headerBytes dataWidth <= dataWidth) =>
  (metaIn -> metaOut) ->
  (metaIn -> header) ->
  PacketizerState metaOut headerBytes dataWidth ->
  (Maybe (PacketStreamM2S dataWidth metaIn), PacketStreamS2M) ->
  ( PacketizerState metaOut headerBytes dataWidth
  , (PacketStreamS2M, Maybe (PacketStreamM2S dataWidth metaOut))
  )
packetizerT toMetaOut toHeader st@Insert{..} (Just pkt@PacketStreamM2S{..}, bwdIn) =
  (nextStOut, (bwdOut, fwdOut))
 where
  alignedCmp = compareSNat (SNat @(ForwardBufSize headerBytes dataWidth)) d0
  nextAborted = _aborted || _abort
  header = bitCoerce (toHeader _meta)
  metaOut = toMetaOut _meta
  hdrBuf = if _counter == 0 then header ++ _data else _hdrBuf
  (newHdrBuf, dataOut) = shiftOutFrom0 (SNat @dataWidth) hdrBuf
  forwardBytes = snd $ shiftOutFromN (SNat @(ForwardBufSize headerBytes dataWidth)) _data

  newLast = case alignedCmp of
    SNatGT -> fmap (adjustLast (SNat @headerBytes)) _last
    _ -> Nothing

  fwdOut =
    Just
      pkt
        { _data = dataOut
        , _last = if _counter == maxBound then either Just (const Nothing) =<< newLast else Nothing
        , _meta = metaOut
        , _abort = nextAborted
        }

  nextSt = case (_counter == maxBound, newLast) of
    (False, _) -> Insert (succ _counter) newHdrBuf nextAborted
    (True, Nothing) -> Forward forwardBytes nextAborted
    (True, Just (Left _)) -> def
    (True, Just (Right idx)) ->
      LastForward
        (PacketStreamM2S (take (SNat @dataWidth) newHdrBuf) (Just idx) metaOut nextAborted)

  nextStOut = if isNothing fwdOut || _ready bwdIn then nextSt else st

  -- Assert backpressure while inserting the header. If shifting needs to be done
  -- and we are at the last cycle of insertion, we do not need to assert backpressure
  -- because we put the rest of the data in _fwdBuf (of course, unless our subordinate asserts backpressure).
  bwdOut = PacketStreamS2M $ case alignedCmp of
    SNatGT -> _ready bwdIn && _counter == maxBound
    _ -> False
packetizerT toMetaOut _ st@Forward{..} (Just pkt@PacketStreamM2S{..}, bwdIn) = (nextStOut, (bwdIn, Just outPkt))
 where
  nextAborted = _aborted || _abort
  metaOut = toMetaOut _meta
  (dataOut, nextFwdBuf) = splitAt (SNat @dataWidth) (_fwdBuf ++ _data)
  dataLast = nextFwdBuf ++ repeat @(dataWidth - ForwardBufSize headerBytes dataWidth) defaultByte
  newLast = fmap (adjustLast (SNat @headerBytes)) _last

  outPkt =
    pkt
      { _data = dataOut
      , _last = either Just (const Nothing) =<< newLast
      , _meta = metaOut
      , _abort = nextAborted
      }

  nextSt = case newLast of
    Nothing -> Forward nextFwdBuf nextAborted
    Just (Left _) -> def
    Just (Right idx) -> LastForward (PacketStreamM2S dataLast (Just idx) metaOut nextAborted)

  nextStOut = if _ready bwdIn then nextSt else st
packetizerT _ _ st@LastForward{..} (_, bwdIn) = (nextStOut, (PacketStreamS2M False, Just _lastFragment))
 where
  nextStOut = if _ready bwdIn then def else st
packetizerT _ _ s (Nothing, bwdIn) = (s, (bwdIn, Nothing))

{- | Puts a portion of the metadata in front of the packet stream, and shifts the stream accordingly.
  This portion is defined by the metadata to header transformer function. If this function is `id`,
  the entire metadata is put in front of the packet stream.
-}
packetizerC ::
  forall
    (dom :: Domain)
    (dataWidth :: Nat)
    (metaIn :: Type)
    (metaOut :: Type)
    (header :: Type)
    (headerBytes :: Nat).
  (HiddenClockResetEnable dom) =>
  (NFDataX metaOut) =>
  (BitPack header) =>
  (BitSize header ~ headerBytes * 8) =>
  (KnownNat headerBytes) =>
  (1 <= dataWidth) =>
  (KnownNat dataWidth) =>
  -- | Metadata transformer function
  (metaIn -> metaOut) ->
  -- | metaData to header that will be packetized transformer function
  (metaIn -> header) ->
  Circuit (PacketStream dom dataWidth metaIn) (PacketStream dom dataWidth metaOut)
packetizerC toMetaOut toHeader = fromSignals outCircuit
 where
  outCircuit = case compareSNat (SNat @(ForwardBufSize headerBytes dataWidth)) (SNat @dataWidth) of
    SNatLE -> mealyB (packetizerT @headerBytes toMetaOut toHeader) def
    _ ->
      clashCompileError
        "packetizerC: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"

{- | If dataWidth >= headerBytes, we don't need a buffer because we can immediately send
  the fragment. Else, we need a buffer that stores the headerBytes minus the size
  of the fragment we send out immediately.
-}
type DfHeaderBufSize headerBytes dataWidth = dataWidth `Max` headerBytes - dataWidth

data DfPacketizerState (metaOut :: Type) (headerBytes :: Nat) (dataWidth :: Nat)
  = DfIdle
  | DfInsert
      { _dfCounter :: Index (headerBytes `DivRU` dataWidth - 1)
      , _dfHdrBuf :: Vec (DfHeaderBufSize headerBytes dataWidth) (BitVector 8)
      }
  deriving (Generic, NFDataX, Show, ShowX)

packetizeFromDfT ::
  forall
    (dataWidth :: Nat)
    (a :: Type)
    (metaOut :: Type)
    (header :: Type)
    (headerBytes :: Nat).
  (NFDataX metaOut) =>
  (BitPack header) =>
  (BitSize header ~ headerBytes * 8) =>
  (KnownNat headerBytes) =>
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  (1 <= headerBytes `DivRU` dataWidth) =>
  (dataWidth `Min` headerBytes <= dataWidth) =>
  (dataWidth `Max` headerBytes - dataWidth + dataWidth `Min` headerBytes ~ headerBytes) =>
  -- | function that transforms the Df input to the output metadata.
  (a -> metaOut) ->
  -- | function that transforms the Df input to the header that will be packetized.
  (a -> header) ->
  DfPacketizerState metaOut headerBytes dataWidth ->
  (Df.Data a, PacketStreamS2M) ->
  ( DfPacketizerState metaOut headerBytes dataWidth
  , (Ack, Maybe (PacketStreamM2S dataWidth metaOut))
  )
packetizeFromDfT toMetaOut toHeader DfIdle (Df.Data dataIn, bwdIn) = (nextStOut, (bwdOut, Just outPkt))
 where
  rotatedHdr =
    rotateRightS (bitCoerce (toHeader dataIn)) (SNat @(DfHeaderBufSize headerBytes dataWidth))
  (hdrBuf, dataOut) = splitAt (SNat @(DfHeaderBufSize headerBytes dataWidth)) rotatedHdr
  dataOutPadded = dataOut ++ repeat @(dataWidth - dataWidth `Min` headerBytes) defaultByte
  outPkt = PacketStreamM2S dataOutPadded newLast (toMetaOut dataIn) False

  (nextSt, bwdOut, newLast) = case compareSNat (SNat @headerBytes) (SNat @dataWidth) of
    SNatLE -> (DfIdle, Ack (_ready bwdIn), Just l)
     where
      l = case compareSNat (SNat @(headerBytes `Mod` dataWidth)) d0 of
        SNatGT -> natToNum @(headerBytes `Mod` dataWidth - 1)
        _ -> natToNum @(dataWidth - 1)
    SNatGT -> (DfInsert 0 hdrBuf, Ack False, Nothing)

  nextStOut = if _ready bwdIn then nextSt else DfIdle

-- fwdIn is always Data in this state, because we assert backpressure in Idle before we go here
-- Thus, we don't need to store the metadata in the state.
packetizeFromDfT toMetaOut _ st@DfInsert{..} (Df.Data dataIn, bwdIn) = (nextStOut, (bwdOut, Just outPkt))
 where
  (dataOut, newHdrBuf) = splitAt (SNat @dataWidth) (_dfHdrBuf ++ repeat @dataWidth defaultByte)
  outPkt = PacketStreamM2S dataOut newLast (toMetaOut dataIn) False

  newLast = toMaybe (_dfCounter == maxBound) $ case compareSNat (SNat @(headerBytes `Mod` dataWidth)) d0 of
    SNatGT -> natToNum @(headerBytes `Mod` dataWidth - 1)
    _ -> natToNum @(dataWidth - 1)

  bwdOut = Ack (_ready bwdIn && _dfCounter == maxBound)
  nextSt = if _dfCounter == maxBound then DfIdle else DfInsert (succ _dfCounter) newHdrBuf
  nextStOut = if _ready bwdIn then nextSt else st
packetizeFromDfT _ _ s (Df.NoData, bwdIn) = (s, (Ack (_ready bwdIn), Nothing))

{- | Starts a packet stream upon receiving some data.
  The bytes to be packetized and the output metadata
  are specified by the input functions.
-}
packetizeFromDfC ::
  forall
    (dom :: Domain)
    (dataWidth :: Nat)
    (a :: Type)
    (metaOut :: Type)
    (header :: Type)
    (headerBytes :: Nat).
  (HiddenClockResetEnable dom) =>
  (NFDataX metaOut) =>
  (BitPack header) =>
  (BitSize header ~ headerBytes * 8) =>
  (KnownNat headerBytes) =>
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  -- | Function that transforms the Df input to the output metadata.
  (a -> metaOut) ->
  -- | Function that transforms the Df input to the header that will be packetized.
  (a -> header) ->
  Circuit (Df dom a) (PacketStream dom dataWidth metaOut)
packetizeFromDfC toMetaOut toHeader = fromSignals ckt
 where
  maxMinProof =
    sameNat
      (SNat @headerBytes)
      (SNat @(dataWidth `Max` headerBytes - dataWidth + dataWidth `Min` headerBytes))
  minProof = compareSNat (SNat @(dataWidth `Min` headerBytes)) (SNat @dataWidth)
  divRuProof = compareSNat d1 (SNat @(headerBytes `DivRU` dataWidth))

  ckt = case (maxMinProof, minProof, divRuProof) of
    (Just Refl, SNatLE, SNatLE) -> mealyB (packetizeFromDfT toMetaOut toHeader) DfIdle
    _ ->
      clashCompileError
        "packetizeFromDfC: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"
