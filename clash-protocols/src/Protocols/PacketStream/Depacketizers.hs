{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

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

import Data.Data ((:~:) (Refl))
import Data.Maybe

defaultByte :: BitVector 8
defaultByte = 0x00

-- Since the header might be unaligned compared to the datawidth
-- we need to store a partial fragment when forwarding.
-- The number of bytes we need to store depends on our "unalignedness".
--
-- Ex. We parse a header of 17 bytes and our @dataWidth@ is 4 bytes.
-- That means at the end of the header we can have upto 3 bytes left
-- in the fragment which we may need to forward.
type ForwardBufSize (headerBytes :: Nat) (dataWidth :: Nat) =
  (dataWidth - (headerBytes `Mod` dataWidth)) `Mod` dataWidth

type DepacketizerCt (headerBytes :: Nat) (dataWidth :: Nat) =
  ( headerBytes `Mod` dataWidth <= dataWidth
  , KnownNat dataWidth
  , 1 <= dataWidth
  , KnownNat headerBytes
  )

-- TODO remove _fwdBuf and just use the last ForwardBufSize bytes of _parseBuf instead
-- See https://github.com/clash-lang/clash-protocols/issues/105
data DepacketizerState (headerBytes :: Nat) (dataWidth :: Nat)
  = Parse
      { _aborted :: Bool
      -- ^ Whether the packet is aborted. We need this, because _abort might
      --   have been set in the bytes to be parsed.
      , _parseBuf :: Vec (dataWidth * headerBytes `DivRU` dataWidth) (BitVector 8)
      -- ^ Parse buffer.
      , _fwdBuf :: Vec (ForwardBufSize headerBytes dataWidth) (BitVector 8)
      -- ^ Buffer containing data bytes that could not be sent immediately
      --   due to misalignment of @dataWidth@ and @headerBytes@.
      , _counter :: Index (headerBytes `DivRU` dataWidth)
      -- ^ @maxBound + 1@ is the number of fragments we need to parse.
      }
  | Forward
      { _aborted :: Bool
      , _parseBuf :: Vec (dataWidth * headerBytes `DivRU` dataWidth) (BitVector 8)
      , _fwdBuf :: Vec (ForwardBufSize headerBytes dataWidth) (BitVector 8)
      , _counter :: Index (headerBytes `DivRU` dataWidth)
      , _lastFwd :: Bool
      -- ^ True iff we have seen @_last@ set but the number of data bytes was too
      --   big to send immediately along with our buffered bytes.
      }
  deriving (Show, ShowX, Generic)

deriving instance
  (DepacketizerCt headerBytes dataWidth) =>
  NFDataX (DepacketizerState headerBytes dataWidth)

-- | Initial state of @depacketizerT@
instance
  (DepacketizerCt headerBytes dataWidth) =>
  Default (DepacketizerState headerBytes dataWidth)
  where
  def :: DepacketizerState headerBytes dataWidth
  def = Parse False (repeat undefined) (repeat undefined) maxBound

depacketizerT ::
  forall
    (headerBytes :: Nat)
    (dataWidth :: Nat)
    (header :: Type)
    (metaIn :: Type)
    (metaOut :: Type).
  (BitSize header ~ headerBytes * 8) =>
  (BitPack header) =>
  (DepacketizerCt headerBytes dataWidth) =>
  (NFDataX metaIn) =>
  (ForwardBufSize headerBytes dataWidth <= dataWidth) =>
  (headerBytes <= dataWidth * headerBytes `DivRU` dataWidth) =>
  (header -> metaIn -> metaOut) ->
  DepacketizerState headerBytes dataWidth ->
  (Maybe (PacketStreamM2S dataWidth metaIn), PacketStreamS2M) ->
  ( DepacketizerState headerBytes dataWidth
  , (PacketStreamS2M, Maybe (PacketStreamM2S dataWidth metaOut))
  )
depacketizerT _ Parse{..} (Just PacketStreamM2S{..}, _) = (nextStOut, (PacketStreamS2M outReady, Nothing))
 where
  nextAborted = _aborted || _abort
  nextCounter = pred _counter
  nextParseBuf = fst (shiftInAtN _parseBuf _data)
  nextFwdBuf = dropLe (SNat @(dataWidth - ForwardBufSize headerBytes dataWidth)) _data

  prematureEnd idx = case sameNat d0 (SNat @(headerBytes `Mod` dataWidth)) of
    Just Refl -> True
    _ -> idx < natToNum @(headerBytes `Mod` dataWidth)

  -- Upon seeing _last being set, move back to the initial state if the
  -- right amount of bytes were not parsed yet, or if they were, but there
  -- were no data bytes after that. In any case, we pass the same buffers
  -- for efficiency reasons, because their initial contents are undefined
  -- anyway.
  nextStOut = case (_counter == 0, _last) of
    (False, Nothing) ->
      Parse nextAborted nextParseBuf nextFwdBuf nextCounter
    (False, Just _) ->
      def
    (True, Just idx)
      | prematureEnd idx ->
          def
    (True, _) ->
      Forward nextAborted nextParseBuf nextFwdBuf nextCounter (isJust _last)

  outReady
    | Forward{_lastFwd = True} <- nextStOut = False
    | otherwise = True
depacketizerT toMetaOut st@Forward{..} (Just pkt@PacketStreamM2S{..}, bwdIn) = (nextStOut, (PacketStreamS2M outReady, Just outPkt))
 where
  nextAborted = _aborted || _abort
  newLast = adjustLast <$> _last
  (dataOut, nextFwdBuf) = splitAt (SNat @dataWidth) (_fwdBuf ++ _data)

  -- Only use if headerBytes `Mod` dataWidth > 0.
  adjustLast :: Index dataWidth -> Either (Index dataWidth) (Index dataWidth)
  adjustLast idx = if idx < x then Left (idx + y) else Right (idx - x)
   where
    x = natToNum @(headerBytes `Mod` dataWidth)
    y = natToNum @(ForwardBufSize headerBytes dataWidth)

  outPkt = case sameNat d0 (SNat @(headerBytes `Mod` dataWidth)) of
    Just Refl ->
      pkt
        { _meta = toMetaOut (bitCoerce $ takeLe (SNat @headerBytes) _parseBuf) _meta
        , _abort = nextAborted
        }
    Nothing ->
      pkt
        { _data =
            if _lastFwd
              then _fwdBuf ++ repeat @(dataWidth - ForwardBufSize headerBytes dataWidth) defaultByte
              else dataOut
        , _last =
            if _lastFwd
              then either Just Just =<< newLast
              else either Just (const Nothing) =<< newLast
        , _meta = toMetaOut (bitCoerce $ takeLe (SNat @headerBytes) _parseBuf) _meta
        , _abort = nextAborted
        }

  nextForwardSt = Forward nextAborted _parseBuf nextFwdBuf maxBound
  nextSt = case sameNat d0 (SNat @(headerBytes `Mod` dataWidth)) of
    Just Refl
      | isJust _last -> def
      | otherwise -> nextForwardSt False
    Nothing ->
      case (_lastFwd, newLast) of
        (False, Nothing) -> nextForwardSt False
        (False, Just (Right _)) -> nextForwardSt True
        _ -> def

  nextStOut = if _ready bwdIn then nextSt else st

  outReady
    | Forward{_lastFwd = True} <- nextStOut = False
    | otherwise = _ready bwdIn
depacketizerT _ st (Nothing, bwdIn) = (st, (bwdIn, Nothing))

-- | Reads bytes at the start of each packet into metadata.
depacketizerC ::
  forall
    (dom :: Domain)
    (dataWidth :: Nat)
    (metaIn :: Type)
    (metaOut :: Type)
    (header :: Type)
    (headerBytes :: Nat).
  ( HiddenClockResetEnable dom
  , NFDataX metaOut
  , NFDataX metaIn
  , BitPack header
  , BitSize header ~ headerBytes * 8
  , KnownNat headerBytes
  , 1 <= dataWidth
  , KnownNat dataWidth
  ) =>
  -- | Used to compute final metadata of outgoing packets from header and incoming metadata
  (header -> metaIn -> metaOut) ->
  Circuit (PacketStream dom dataWidth metaIn) (PacketStream dom dataWidth metaOut)
depacketizerC toMetaOut = forceResetSanity |> fromSignals outCircuit
 where
  modProof = compareSNat (SNat @(headerBytes `Mod` dataWidth)) (SNat @dataWidth)
  divProof = compareSNat (SNat @headerBytes) (SNat @(dataWidth * headerBytes `DivRU` dataWidth))

  outCircuit =
    case (modProof, divProof) of
      (SNatLE, SNatLE) -> case compareSNat (SNat @(ForwardBufSize headerBytes dataWidth)) (SNat @dataWidth) of
        SNatLE -> mealyB (depacketizerT @headerBytes toMetaOut) def
        _ ->
          clashCompileError
            "depacketizer1: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"
      _ ->
        clashCompileError
          "depacketizer0: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"

type DepacketizeToDfCt (headerBytes :: Nat) (dataWidth :: Nat) =
  ( 1 <= headerBytes `DivRU` dataWidth
  , headerBytes `Mod` dataWidth <= dataWidth
  , headerBytes <= dataWidth * headerBytes `DivRU` dataWidth
  , KnownNat dataWidth
  , 1 <= dataWidth
  , KnownNat headerBytes
  )

data DfDepacketizerState (headerBytes :: Nat) (dataWidth :: Nat)
  = DfParse
      { _dfAborted :: Bool
      -- ^ whether any of the fragments parsed from the current packet were aborted.
      , _dfParseBuf :: Vec (dataWidth * headerBytes `DivRU` dataWidth) (BitVector 8)
      -- ^ the accumulator for header bytes.
      , _dfCounter :: Index (headerBytes `DivRU` dataWidth)
      -- ^ how many of the _parseBuf bytes are currently valid (accumulation count). We flush at counter == maxBound
      }
  | DfConsumePadding
      { _dfAborted :: Bool
      -- ^ whether any of the fragments parsed from the current packet were aborted.
      , _dfParseBuf :: Vec (dataWidth * headerBytes `DivRU` dataWidth) (BitVector 8)
      }
  deriving (Generic, Show, ShowX)

deriving instance
  (DepacketizeToDfCt headerBytes dataWidth) =>
  (NFDataX (DfDepacketizerState headerBytes dataWidth))

-- | Initial state of @depacketizeToDfT@
instance
  (DepacketizeToDfCt headerBytes dataWidth) =>
  Default (DfDepacketizerState headerBytes dataWidth)
  where
  def :: DfDepacketizerState headerBytes dataWidth
  def = DfParse False (repeat undefined) maxBound

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
  DfDepacketizerState headerBytes dataWidth ->
  (Maybe (PacketStreamM2S dataWidth meta), Ack) ->
  (DfDepacketizerState headerBytes dataWidth, (PacketStreamS2M, Df.Data a))
depacketizeToDfT toOut st@DfParse{..} (Just (PacketStreamM2S{..}), Ack readyIn) = (nextStOut, (PacketStreamS2M readyOut, fwdOut))
 where
  nextAborted = _dfAborted || _abort
  nextParseBuf = fst (shiftInAtN _dfParseBuf _data)
  outDf = toOut (bitCoerce (takeLe (SNat @headerBytes) nextParseBuf)) _meta

  prematureEnd idx =
    case compareSNat (SNat @(headerBytes `Mod` dataWidth)) d0 of
      SNatGT -> idx < (natToNum @(headerBytes `Mod` dataWidth - 1))
      _ -> idx < (natToNum @(dataWidth - 1))

  (nextSt, fwdOut) =
    case (_dfCounter == 0, _last) of
      (False, Nothing) ->
        (DfParse nextAborted nextParseBuf (pred _dfCounter), Df.NoData)
      (c, Just idx)
        | not c || prematureEnd idx ->
            (def, Df.NoData)
      (True, Just _) ->
        (def, if nextAborted then Df.NoData else Df.Data outDf)
      (True, Nothing) ->
        (DfConsumePadding nextAborted nextParseBuf, Df.NoData)
      _ ->
        clashCompileError
          "depacketizeToDfT Parse: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"

  readyOut = isNothing (Df.dataToMaybe fwdOut) || readyIn
  nextStOut = if readyOut then nextSt else st
depacketizeToDfT toOut st@DfConsumePadding{..} (Just (PacketStreamM2S{..}), Ack readyIn) = (nextStOut, (PacketStreamS2M readyOut, fwdOut))
 where
  nextAborted = _dfAborted || _abort
  outDf = toOut (bitCoerce (takeLe (SNat @headerBytes) _dfParseBuf)) _meta

  (nextSt, fwdOut) =
    if isJust _last
      then (def, if nextAborted then Df.NoData else Df.Data outDf)
      else (st{_dfAborted = nextAborted}, Df.NoData)

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
        SNatLE -> mealyB (depacketizeToDfT toOut) def
        _ ->
          clashCompileError
            "depacketizeToDfC0: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"
      _ ->
        clashCompileError
          "depacketizeToDfC1: Absurd, Report this to the Clash compiler team: https://github.com/clash-lang/clash-compiler/issues"
