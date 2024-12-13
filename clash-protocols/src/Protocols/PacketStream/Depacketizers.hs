{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=5 #-}
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
Copyright  :  (C) 2024, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Utility circuits for reading from a packet stream.
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

import Data.Constraint (Dict (Dict))
import Data.Constraint.Nat.Extra
import Data.Data ((:~:) (Refl))
import Data.Maybe

{- | Vectors of this size are able to hold @headerBytes `DivRU` dataWidth@
     transfers of size @dataWidth@, which is bigger than or equal to @headerBytes@.
-}
type BufSize (headerBytes :: Nat) (dataWidth :: Nat) =
  dataWidth * headerBytes `DivRU` dataWidth

{- | Since the header might be unaligned compared to the data width
     we need to store a partial fragment when forwarding.
     The number of bytes we need to store depends on our "unalignedness".

     Ex. We parse a header of 17 bytes and our @dataWidth@ is 4 bytes.
     That means at the end of the header we can have up to 3 bytes left
     in the fragment which we may need to forward.
-}
type ForwardBytes (headerBytes :: Nat) (dataWidth :: Nat) =
  (dataWidth - (headerBytes `Mod` dataWidth)) `Mod` dataWidth

-- | Depacketizer constraints.
type DepacketizerCt (headerBytes :: Nat) (dataWidth :: Nat) =
  ( KnownNat headerBytes
  , KnownNat dataWidth
  , 1 <= headerBytes
  , 1 <= dataWidth
  , BufSize headerBytes dataWidth ~ headerBytes + ForwardBytes headerBytes dataWidth
  , headerBytes `Mod` dataWidth <= dataWidth
  , ForwardBytes headerBytes dataWidth <= dataWidth
  )

{- | Depacketizer state. Either we are parsing a header, or we are forwarding
  the rest of the packet along with the parsed header in its metadata.
-}
data DepacketizerState (headerBytes :: Nat) (dataWidth :: Nat)
  = Parse
      { _aborted :: Bool
      -- ^ Whether the packet is aborted. We need this, because _abort might
      --   have been set in the bytes to be parsed.
      , _buf :: Vec (BufSize headerBytes dataWidth) (BitVector 8)
      -- ^ The first @headerBytes@ of this buffer are for the parsed header.
      --   The bytes after that are data bytes that could not be sent immediately
      --   due to misalignment of @dataWidth@ and @headerBytes@.
      , _counter :: Index (headerBytes `DivRU` dataWidth)
      -- ^ @maxBound + 1@ is the number of fragments we need to parse.
      }
  | Forward
      { _aborted :: Bool
      , _buf :: Vec (BufSize headerBytes dataWidth) (BitVector 8)
      , _counter :: Index (headerBytes `DivRU` dataWidth)
      , _lastFwd :: Bool
      -- ^ True iff we have seen @_last@ set but the number of data bytes was too
      --   big to send immediately along with our buffered bytes.
      }
  deriving (Show, ShowX, Generic)

deriving instance
  (DepacketizerCt headerBytes dataWidth) =>
  NFDataX (DepacketizerState headerBytes dataWidth)

-- | Initial state of @depacketizerT@.
instance
  (DepacketizerCt headerBytes dataWidth) =>
  Default (DepacketizerState headerBytes dataWidth)
  where
  def :: DepacketizerState headerBytes dataWidth
  def = Parse False (deepErrorX "depacketizerT: undefined initial buffer") maxBound

-- | Depacketizer state transition function.
depacketizerT ::
  forall
    (headerBytes :: Nat)
    (header :: Type)
    (metaIn :: Type)
    (metaOut :: Type)
    (dataWidth :: Nat).
  (BitPack header) =>
  (BitSize header ~ headerBytes * 8) =>
  (NFDataX metaIn) =>
  (DepacketizerCt headerBytes dataWidth) =>
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
  nextParseBuf = fst (shiftInAtN _buf _data)

  prematureEnd idx = case sameNat d0 (SNat @(headerBytes `Mod` dataWidth)) of
    Just Refl -> idx /= maxBound
    _ -> idx < natToNum @(headerBytes `Mod` dataWidth)

  -- Upon seeing _last being set, move back to the initial state if the
  -- right amount of bytes were not parsed yet, or if they were, but there
  -- were no data bytes after that.
  nextStOut = case (_counter == 0, _last) of
    (False, Nothing) ->
      Parse nextAborted nextParseBuf nextCounter
    (False, Just _) ->
      def
    (True, Just idx)
      | prematureEnd idx ->
          def
    (True, _) ->
      Forward nextAborted nextParseBuf nextCounter (isJust _last)

  outReady
    | Forward{_lastFwd = True} <- nextStOut = False
    | otherwise = True
depacketizerT toMetaOut st@Forward{..} (Just pkt@PacketStreamM2S{..}, bwdIn) = (nextStOut, (PacketStreamS2M outReady, Just outPkt))
 where
  nextAborted = _aborted || _abort
  nextBuf = header ++ nextFwdBytes
  newLast = adjustLast <$> _last
  (header, fwdBytes) = splitAt (SNat @headerBytes) _buf
  (dataOut, nextFwdBytes) = splitAt (SNat @dataWidth) (fwdBytes ++ _data)

  -- Only use if headerBytes `Mod` dataWidth > 0.
  adjustLast ::
    Index (dataWidth + 1) -> Either (Index (dataWidth + 1)) (Index (dataWidth + 1))
  adjustLast idx
    | _lastFwd = Right (idx - x)
    | idx <= x = Left (idx + y)
    | otherwise = Right (idx - x)
   where
    x = natToNum @(headerBytes `Mod` dataWidth)
    y = natToNum @(ForwardBytes headerBytes dataWidth)

  outPkt = case sameNat d0 (SNat @(headerBytes `Mod` dataWidth)) of
    Just Refl ->
      pkt
        { _data = _data
        , _last = if _lastFwd then Just 0 else _last
        , _meta = toMetaOut (bitCoerce header) _meta
        , _abort = nextAborted
        }
    Nothing ->
      pkt
        { _data = dataOut
        , _last =
            if _lastFwd
              then either Just Just =<< newLast
              else either Just (const Nothing) =<< newLast
        , _meta = toMetaOut (bitCoerce header) _meta
        , _abort = nextAborted
        }

  nextForwardSt = Forward nextAborted nextBuf maxBound
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

{- |
Reads bytes at the start of each packet into `_meta`. If a packet contains
less valid bytes than @headerBytes + 1@, it will silently drop that packet.

If @dataWidth@ divides @headerBytes@, this component runs at full throughput.
Otherwise, it gives backpressure for one clock cycle per packet larger than
@headerBytes + 1@ valid bytes.
-}
depacketizerC ::
  forall
    (headerBytes :: Nat)
    (header :: Type)
    (metaIn :: Type)
    (metaOut :: Type)
    (dataWidth :: Nat)
    (dom :: Domain).
  (HiddenClockResetEnable dom) =>
  (BitPack header) =>
  (BitSize header ~ headerBytes * 8) =>
  (NFDataX metaIn) =>
  (KnownNat headerBytes) =>
  (KnownNat dataWidth) =>
  (1 <= headerBytes) =>
  (1 <= dataWidth) =>
  -- |  Mapping from the parsed header and input `_meta` to the output `_meta`
  (header -> metaIn -> metaOut) ->
  Circuit (PacketStream dom dataWidth metaIn) (PacketStream dom dataWidth metaOut)
depacketizerC toMetaOut = forceResetSanity |> fromSignals ckt
 where
  ckt = case ( eqTimesDivRu @dataWidth @headerBytes
             , leModulusDivisor @headerBytes @dataWidth
             , leModulusDivisor @(dataWidth - (headerBytes `Mod` dataWidth)) @dataWidth
             ) of
    (Dict, Dict, Dict) -> mealyB (depacketizerT toMetaOut) def

-- | Df depacketizer constraints.
type DepacketizeToDfCt (headerBytes :: Nat) (dataWidth :: Nat) =
  ( KnownNat headerBytes
  , KnownNat dataWidth
  , 1 <= headerBytes
  , 1 <= dataWidth
  , headerBytes <= headerBytes `DivRU` dataWidth * dataWidth
  )

data DfDepacketizerState (headerBytes :: Nat) (dataWidth :: Nat)
  = DfParse
      { _dfAborted :: Bool
      -- ^ Whether the current packet is aborted. We need this, because _abort
      --   might have been set in the bytes to be parsed.
      , _dfParseBuf :: Vec (BufSize headerBytes dataWidth) (BitVector 8)
      -- ^ Buffer for the header that we need to parse.
      , _dfCounter :: Index (headerBytes `DivRU` dataWidth)
      -- ^ @maxBound + 1@ is the number of fragments we need to parse.
      }
  | DfConsumePadding
      { _dfAborted :: Bool
      , _dfParseBuf :: Vec (BufSize headerBytes dataWidth) (BitVector 8)
      }
  deriving (Generic, Show, ShowX)

deriving instance
  (DepacketizeToDfCt headerBytes dataWidth) =>
  (NFDataX (DfDepacketizerState headerBytes dataWidth))

-- | Initial state of @depacketizeToDfT@.
instance
  (DepacketizeToDfCt headerBytes dataWidth) =>
  Default (DfDepacketizerState headerBytes dataWidth)
  where
  def :: DfDepacketizerState headerBytes dataWidth
  def = DfParse False (deepErrorX "depacketizeToDfT: undefined initial buffer") maxBound

-- | Df depacketizer transition function.
depacketizeToDfT ::
  forall
    (headerBytes :: Nat)
    (header :: Type)
    (meta :: Type)
    (a :: Type)
    (dataWidth :: Nat).
  (NFDataX meta) =>
  (BitPack header) =>
  (BitSize header ~ headerBytes * 8) =>
  (DepacketizeToDfCt headerBytes dataWidth) =>
  (header -> meta -> a) ->
  DfDepacketizerState headerBytes dataWidth ->
  (Maybe (PacketStreamM2S dataWidth meta), Ack) ->
  (DfDepacketizerState headerBytes dataWidth, (PacketStreamS2M, Df.Data a))
depacketizeToDfT _ DfParse{..} (Just (PacketStreamM2S{..}), _) = (nextStOut, (PacketStreamS2M readyOut, Df.NoData))
 where
  nextAborted = _dfAborted || _abort
  nextParseBuf = fst (shiftInAtN _dfParseBuf _data)

  prematureEnd idx =
    case compareSNat (SNat @(headerBytes `Mod` dataWidth)) d0 of
      SNatGT -> idx < (natToNum @(headerBytes `Mod` dataWidth))
      _ -> idx < (natToNum @dataWidth)

  (nextStOut, readyOut) =
    case (_dfCounter == 0, _last) of
      (False, Nothing) ->
        (DfParse nextAborted nextParseBuf (pred _dfCounter), True)
      (False, Just _) ->
        (def, True)
      (True, Just idx) ->
        if nextAborted || prematureEnd idx
          then (def, True)
          else (DfConsumePadding nextAborted nextParseBuf, False)
      (True, Nothing) ->
        (DfConsumePadding nextAborted nextParseBuf, True)
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
depacketizeToDfT _ st (Nothing, Ack readyIn) = (st, (PacketStreamS2M readyIn, Df.NoData))

{- |
Reads bytes at the start of each packet into a header structure, and
transforms this header structure along with the input metadata to an output
structure @a@, which is transmitted over a @Df@ channel. Such a structure
is sent once per packet over the @Df@ channel, but only if the packet was big
enough (contained at least @headerBytes@ valid data bytes) and did not
contain any transfer with @_abort@ set.
The remainder of the packet (padding) is dropped.

If the packet is not padded, or if the packet is padded but the padding fits
in the last transfer of the packet together with valid data bytes, this
component will give one clock cycle of backpressure per packet (for efficiency
reasons). Otherwise, it runs at full throughput.
-}
depacketizeToDfC ::
  forall
    (headerBytes :: Nat)
    (header :: Type)
    (meta :: Type)
    (a :: Type)
    (dataWidth :: Nat)
    (dom :: Domain).
  (HiddenClockResetEnable dom) =>
  (NFDataX meta) =>
  (NFDataX a) =>
  (BitPack header) =>
  (KnownNat headerBytes) =>
  (KnownNat dataWidth) =>
  (1 <= headerBytes) =>
  (1 <= dataWidth) =>
  (BitSize header ~ headerBytes * 8) =>
  -- | Mapping from the parsed header and input `_meta` to the `Df` output
  (header -> meta -> a) ->
  Circuit (PacketStream dom dataWidth meta) (Df dom a)
depacketizeToDfC toOut = forceResetSanity |> fromSignals ckt
 where
  ckt = case leTimesDivRu @dataWidth @headerBytes of
    Dict -> mealyB (depacketizeToDfT toOut) def
