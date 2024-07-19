{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Utility circuits for stripping headers from the beginning of packets.
-}
module Protocols.PacketStream.Depacketizers (
  depacketizerC,
  depacketizeToDfC,
) where

import Clash.Prelude
import Clash.Sized.Vector.Extra

import Protocols
import qualified Protocols.Df as Df
import Protocols.PacketStream.Base

import Data.Maybe

defaultByte :: BitVector 8
defaultByte = 0x00

-- Since the header might be unaligned compared to the datawidth
-- we need to store a partial fragment when forwarding.
-- The fragment we need to store depends on our "unalignedness".
--
-- Ex. We parse a header of 17 bytes and our @dataWidth@ is 4 bytes.
-- That means at the end of the header we can have upto 3 bytes left
-- in the fragment which we may need to forward.
type DeForwardBufSize (headerBytes :: Nat) (dataWidth :: Nat) =
  (dataWidth - (headerBytes `Mod` dataWidth)) `Mod` dataWidth

type DepacketizerCt (headerBytes :: Nat) (dataWidth :: Nat) =
  ( headerBytes `Mod` dataWidth <= dataWidth
  , KnownNat dataWidth
  , 1 <= dataWidth
  , KnownNat headerBytes
  )

data DepacketizerState (headerBytes :: Nat) (dataWidth :: Nat)
  = DeParse
      { _deAborted :: Bool
      , _deParseBuf :: Vec (dataWidth * headerBytes `DivRU` dataWidth) (BitVector 8)
      , _deFwdBuf :: Vec (DeForwardBufSize headerBytes dataWidth) (BitVector 8)
      , _deCounter :: Index (headerBytes `DivRU` dataWidth)
      }
  | DeForward
      { _deAborted :: Bool
      , _deParseBuf :: Vec (dataWidth * headerBytes `DivRU` dataWidth) (BitVector 8)
      , _deFwdBuf :: Vec (DeForwardBufSize headerBytes dataWidth) (BitVector 8)
      , _deCounter :: Index (headerBytes `DivRU` dataWidth)
      }
  | DeLastForward
      { _deAborted :: Bool
      , _deParseBuf :: Vec (dataWidth * headerBytes `DivRU` dataWidth) (BitVector 8)
      , _deFwdBuf :: Vec (DeForwardBufSize headerBytes dataWidth) (BitVector 8)
      , _deCounter :: Index (headerBytes `DivRU` dataWidth)
      , _deLastIdx :: Index dataWidth
      }
  deriving (Show, ShowX, Generic)

deriving instance
  (DepacketizerCt headerBytes dataWidth) =>
  NFDataX (DepacketizerState headerBytes dataWidth)

depacketizerT ::
  forall
    (headerBytes :: Nat)
    (dataWidth :: Nat)
    (header :: Type)
    (metaIn :: Type)
    (metaOut :: Type).
  (BitSize header ~ headerBytes * 8) =>
  (BitPack header) =>
  (NFDataX metaIn) =>
  (DepacketizerCt headerBytes dataWidth) =>
  (DeForwardBufSize headerBytes dataWidth <= dataWidth) =>
  (headerBytes <= dataWidth * headerBytes `DivRU` dataWidth) =>
  (header -> metaIn -> metaOut) ->
  DepacketizerState headerBytes dataWidth ->
  (Maybe (PacketStreamM2S dataWidth metaIn), PacketStreamS2M) ->
  ( DepacketizerState headerBytes dataWidth
  , (PacketStreamS2M, Maybe (PacketStreamM2S dataWidth metaOut))
  )
depacketizerT _ DeParse{..} (Just PacketStreamM2S{..}, _) = (nextSt, (PacketStreamS2M outReady, Nothing))
 where
  nextAborted = _deAborted || _abort
  nextParseBuf = fst $ shiftInAtN _deParseBuf _data
  fwdBuf :: Vec (DeForwardBufSize headerBytes dataWidth) (BitVector 8)
  fwdBuf = dropLe (SNat @(dataWidth - DeForwardBufSize headerBytes dataWidth)) _data

  prematureEnd idx =
    case compareSNat (SNat @(headerBytes `Mod` dataWidth)) d0 of
      SNatGT -> idx < (natToNum @(headerBytes `Mod` dataWidth))
      _ -> True

  nextCounter = pred _deCounter

  nextSt =
    case (_deCounter == 0, _last) of
      (False, Nothing) ->
        DeParse nextAborted nextParseBuf fwdBuf nextCounter
      (done, Just idx)
        | not done || prematureEnd idx ->
            DeParse False nextParseBuf fwdBuf maxBound
      (True, Just idx) ->
        DeLastForward
          nextAborted
          nextParseBuf
          fwdBuf
          nextCounter
          (idx - natToNum @(headerBytes `Mod` dataWidth))
      (True, Nothing) ->
        DeForward nextAborted nextParseBuf fwdBuf nextCounter
      _ ->
        clashCompileError
          "depacketizerT Parse: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"

  outReady
    | DeLastForward{} <- nextSt = False
    | otherwise = True
depacketizerT _ st@DeParse{} (Nothing, bwdIn) = (st, (bwdIn, Nothing))
depacketizerT toMetaOut st@DeForward{..} (Just pkt@PacketStreamM2S{..}, bwdIn) = (nextStOut, (PacketStreamS2M outReady, Just outPkt))
 where
  nextAborted = _deAborted || _abort
  dataOut :: Vec dataWidth (BitVector 8)
  nextFwdBuf :: Vec (DeForwardBufSize headerBytes dataWidth) (BitVector 8)
  (dataOut, nextFwdBuf) = splitAt (SNat @dataWidth) $ _deFwdBuf ++ _data

  deAdjustLast :: Index dataWidth -> Either (Index dataWidth) (Index dataWidth)
  deAdjustLast idx = if outputNow then Left nowIdx else Right nextIdx
   where
    outputNow = case compareSNat (SNat @(headerBytes `Mod` dataWidth)) d0 of
      SNatGT -> idx < natToNum @(headerBytes `Mod` dataWidth)
      _ -> True
    nowIdx = idx + natToNum @(DeForwardBufSize headerBytes dataWidth)
    nextIdx = idx - natToNum @(headerBytes `Mod` dataWidth)

  newLast = fmap deAdjustLast _last
  outPkt =
    pkt
      { _abort = nextAborted
      , _data = dataOut
      , _meta = toMetaOut (bitCoerce $ takeLe (SNat @headerBytes) _deParseBuf) _meta
      , _last = either Just (const Nothing) =<< newLast
      }

  nextSt = case newLast of
    Nothing -> DeForward nextAborted _deParseBuf nextFwdBuf _deCounter
    Just (Left _) -> DeParse False _deParseBuf nextFwdBuf maxBound
    Just (Right idx) -> DeLastForward nextAborted _deParseBuf nextFwdBuf _deCounter idx
  nextStOut = if _ready bwdIn then nextSt else st

  outReady
    | DeLastForward{} <- nextSt = False
    | otherwise = _ready bwdIn
depacketizerT _ st@DeForward{} (Nothing, bwdIn) = (st, (bwdIn, Nothing))
depacketizerT toMetaOut st@DeLastForward{..} (fwdIn, bwdIn) = (nextStOut, (bwdIn, Just outPkt))
 where
  -- We can only get in this state if the previous clock cycle we received a fwdIn
  -- which was also the last fragment
  inPkt = fromJustX fwdIn
  outPkt =
    PacketStreamM2S
      { _abort = _deAborted || _abort inPkt
      , _data =
          _deFwdBuf ++ repeat @(dataWidth - DeForwardBufSize headerBytes dataWidth) defaultByte
      , _meta = toMetaOut (bitCoerce $ takeLe (SNat @headerBytes) _deParseBuf) (_meta inPkt)
      , _last = Just $ fromJustX (_last inPkt) - natToNum @(headerBytes `Mod` dataWidth)
      }
  nextStOut = if _ready bwdIn then DeParse False _deParseBuf _deFwdBuf maxBound else st

-- | Reads bytes at the start of each packet into metadata.
depacketizerC ::
  forall
    (dom :: Domain)
    (dataWidth :: Nat)
    (metaIn :: Type)
    (metaOut :: Type)
    (header :: Type)
    (headerBytes :: Nat).
  (HiddenClockResetEnable dom) =>
  (NFDataX metaOut) =>
  (NFDataX metaIn) =>
  (BitPack header) =>
  (BitSize header ~ headerBytes * 8) =>
  (KnownNat headerBytes) =>
  (1 <= dataWidth) =>
  (KnownNat dataWidth) =>
  -- | Used to compute final metadata of outgoing packets from header and incoming metadata
  (header -> metaIn -> metaOut) ->
  Circuit (PacketStream dom dataWidth metaIn) (PacketStream dom dataWidth metaOut)
depacketizerC toMetaOut = forceResetSanity |> fromSignals outCircuit
 where
  modProof = compareSNat (SNat @(headerBytes `Mod` dataWidth)) (SNat @dataWidth)
  divProof = compareSNat (SNat @headerBytes) (SNat @(dataWidth * headerBytes `DivRU` dataWidth))

  outCircuit =
    case (modProof, divProof) of
      (SNatLE, SNatLE) -> case compareSNat (SNat @(DeForwardBufSize headerBytes dataWidth)) (SNat @dataWidth) of
        SNatLE ->
          mealyB
            (depacketizerT @headerBytes toMetaOut)
            (DeParse False (repeat undefined) (repeat undefined) maxBound)
        _ ->
          clashCompileError
            "depacketizerC1: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"
      _ ->
        clashCompileError
          "depacketizerC0: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"

type ParseBufSize (headerBytes :: Nat) (dataWidth :: Nat) =
  dataWidth * headerBytes `DivRU` dataWidth

type DepacketizeToDfCt (headerBytes :: Nat) (dataWidth :: Nat) =
  ( 1 <= headerBytes `DivRU` dataWidth
  , headerBytes `Mod` dataWidth <= dataWidth
  , headerBytes <= ParseBufSize headerBytes dataWidth
  , KnownNat dataWidth
  , 1 <= dataWidth
  , KnownNat headerBytes
  )

data DfDepacketizerState (a :: Type) (headerBytes :: Nat) (dataWidth :: Nat)
  = Parse
      { _dfDeAborted :: Bool
      -- ^ whether any of the fragments parsed from the current packet were aborted.
      , _dfDeParseBuf :: Vec (ParseBufSize headerBytes dataWidth) (BitVector 8)
      -- ^ the accumulator for header bytes.
      , _dfDeCounter :: Index (headerBytes `DivRU` dataWidth)
      -- ^ how many of the _parseBuf bytes are currently valid (accumulation count). We flush at counter == maxBound
      }
  | ConsumePadding
      { _dfDeAborted :: Bool
      -- ^ whether any of the fragments parsed from the current packet were aborted.
      , _dfDeParseBuf :: Vec (ParseBufSize headerBytes dataWidth) (BitVector 8)
      }
  deriving (Generic, Show, ShowX)

deriving instance
  (NFDataX a, DepacketizeToDfCt headerBytes dataWidth) =>
  NFDataX (DfDepacketizerState a headerBytes dataWidth)

dfDeInitialState ::
  forall (a :: Type) (headerBytes :: Nat) (dataWidth :: Nat).
  (KnownNat dataWidth) =>
  (KnownNat headerBytes) =>
  (1 <= dataWidth) =>
  DfDepacketizerState a headerBytes dataWidth
dfDeInitialState = Parse False (repeat undefined) maxBound

depacketizeToDfT ::
  forall
    (dom :: Domain)
    (dataWidth :: Nat)
    (a :: Type)
    (meta :: Type)
    (header :: Type)
    (headerBytes :: Nat).
  (HiddenClockResetEnable dom) =>
  (NFDataX meta) =>
  (BitPack header) =>
  (BitSize header ~ headerBytes * 8) =>
  (DepacketizeToDfCt headerBytes dataWidth) =>
  (header -> meta -> a) ->
  DfDepacketizerState a headerBytes dataWidth ->
  (Maybe (PacketStreamM2S dataWidth meta), Ack) ->
  (DfDepacketizerState a headerBytes dataWidth, (PacketStreamS2M, Df.Data a))
depacketizeToDfT toOut st@Parse{..} (Just (PacketStreamM2S{..}), Ack readyIn) = (nextStOut, (PacketStreamS2M readyOut, fwdOut))
 where
  nextAborted = _dfDeAborted || _abort
  nextParseBuf = fst (shiftInAtN _dfDeParseBuf _data)
  outDf = toOut (bitCoerce (takeLe (SNat @headerBytes) nextParseBuf)) _meta

  prematureEnd idx =
    case compareSNat (SNat @(headerBytes `Mod` dataWidth)) d0 of
      SNatGT -> idx < (natToNum @(headerBytes `Mod` dataWidth - 1))
      _ -> idx < (natToNum @(dataWidth - 1))

  (nextSt, fwdOut) =
    case (_dfDeCounter == 0, _last) of
      (False, Nothing) ->
        (Parse nextAborted nextParseBuf (pred _dfDeCounter), Df.NoData)
      (c, Just idx)
        | not c || prematureEnd idx ->
            (dfDeInitialState, Df.NoData)
      (True, Just _) ->
        (dfDeInitialState, if nextAborted then Df.NoData else Df.Data outDf)
      (True, Nothing) ->
        (ConsumePadding nextAborted nextParseBuf, Df.NoData)
      _ ->
        clashCompileError
          "depacketizeToDfT Parse: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"

  readyOut = isNothing (Df.dataToMaybe fwdOut) || readyIn
  nextStOut = if readyOut then nextSt else st
depacketizeToDfT toOut st@ConsumePadding{..} (Just (PacketStreamM2S{..}), Ack readyIn) = (nextStOut, (PacketStreamS2M readyOut, fwdOut))
 where
  nextAborted = _dfDeAborted || _abort
  outDf = toOut (bitCoerce (takeLe (SNat @headerBytes) _dfDeParseBuf)) _meta

  (nextSt, fwdOut) =
    if isJust _last
      then (dfDeInitialState, if nextAborted then Df.NoData else Df.Data outDf)
      else (st{_dfDeAborted = nextAborted}, Df.NoData)

  readyOut = isNothing (Df.dataToMaybe fwdOut) || readyIn
  nextStOut = if readyOut then nextSt else st
depacketizeToDfT _ st (Nothing, Ack ready) = (st, (PacketStreamS2M ready, Df.NoData))

{- | Reads bytes at the start of each packet into a dataflow.
Consumes the remainder of the packet and drops this. If a
packet ends sooner than the assumed length of the header,
`depacketizeToDfC` does not send out anything.
If any of the fragments in the packet has _abort set, it drops
the entire packet.
-}
depacketizeToDfC ::
  forall
    (dom :: Domain)
    (dataWidth :: Nat)
    (a :: Type)
    (meta :: Type)
    (header :: Type)
    (headerBytes :: Nat).
  (HiddenClockResetEnable dom) =>
  (NFDataX meta) =>
  (NFDataX a) =>
  (BitPack header) =>
  (KnownNat headerBytes) =>
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  (BitSize header ~ headerBytes * 8) =>
  -- | function that transforms the given meta + parsed header to the output Df
  (header -> meta -> a) ->
  Circuit (PacketStream dom dataWidth meta) (Df dom a)
depacketizeToDfC toOut = forceResetSanity |> fromSignals outCircuit
 where
  divProof = compareSNat (SNat @headerBytes) (SNat @(dataWidth * headerBytes `DivRU` dataWidth))
  modProof = compareSNat (SNat @(headerBytes `Mod` dataWidth)) (SNat @dataWidth)

  outCircuit =
    case (divProof, modProof) of
      (SNatLE, SNatLE) -> case compareSNat d1 (SNat @(headerBytes `DivRU` dataWidth)) of
        SNatLE -> mealyB (depacketizeToDfT toOut) dfDeInitialState
        _ ->
          clashCompileError
            "depacketizeToDfC0: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"
      _ ->
        clashCompileError
          "depacketizeToDfC1: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"
