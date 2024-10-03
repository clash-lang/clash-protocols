{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
Utility circuits for reading from a packet stream.
-}
module Protocols.PacketStream.Depacketizers (
  depacketizerC,
  depacketizeToDfC,
  dropTailC,
) where

import Clash.Prelude
import Clash.Sized.Vector.Extra

import Protocols
import qualified Protocols.Df as Df
import Protocols.PacketStream.Base
import Protocols.PacketStream.Delay

import qualified Data.Bifunctor as B
import Data.Constraint (Dict (Dict))
import Data.Constraint.Nat.Extra
import Data.Data ((:~:) (Refl))
import Data.Maybe

defaultByte :: BitVector 8
defaultByte = 0x00

{- | Vectors of this size are able to hold @headerBytes `DivRU` dataWidth@
     transfers of size @dataWidth@, which is bigger than or equal to @headerBytes@.
-}
type BufSize (headerBytes :: Nat) (dataWidth :: Nat) =
  dataWidth * headerBytes `DivRU` dataWidth

{- | Since the header might be unaligned compared to the datawidth
     we need to store a partial fragment when forwarding.
     The number of bytes we need to store depends on our "unalignedness".

     Ex. We parse a header of 17 bytes and our @dataWidth@ is 4 bytes.
     That means at the end of the header we can have upto 3 bytes left
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
  def = Parse False (repeat undefined) maxBound

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
    Just Refl -> idx == 0
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
  adjustLast idx =
    if _lastFwd then Right (idx - x) else if idx <= x then Left (idx + y) else Right (idx - x)
   where
    x = natToNum @(headerBytes `Mod` dataWidth)
    y = natToNum @(ForwardBytes headerBytes dataWidth)

  outPkt = case sameNat d0 (SNat @(headerBytes `Mod` dataWidth)) of
    Just Refl ->
      pkt
        { _data = if _lastFwd then repeat 0x00 else _data
        , _last = if _lastFwd then Just 0 else _last
        , _meta = toMetaOut (bitCoerce header) _meta
        , _abort = nextAborted
        }
    Nothing ->
      pkt
        { _data =
            if _lastFwd
              then fwdBytes ++ repeat @(dataWidth - ForwardBytes headerBytes dataWidth) defaultByte
              else dataOut
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
  def = DfParse False (repeat undefined) maxBound

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

-- | Information about the tail of a packet, for dropping purposes.
data DropTailInfo dataWidth delay = DropTailInfo
  { _dtAborted :: Bool
  -- ^ Whether any fragment of the packet was aborted
  , _newIdx :: Index (dataWidth + 1)
  -- ^ The adjusted byte enable
  , _drops :: Index (delay + 1)
  -- ^ The amount of transfers to drop from the tail
  , _wait :: Bool
  -- ^ Iff true, apply changes to transfer the next clock cycle instead
  }

{- |
Transmits information about a single packet upon seeing its last transfer.
-}
transmitDropInfoC ::
  forall (dataWidth :: Nat) (delay :: Nat) (meta :: Type) (dom :: Domain) (n :: Nat).
  (KnownNat dataWidth) =>
  (KnownNat delay) =>
  (1 <= dataWidth) =>
  (1 <= n) =>
  (HiddenClockResetEnable dom) =>
  SNat n ->
  Circuit
    (PacketStream dom dataWidth meta)
    (Df dom (DropTailInfo dataWidth delay))
transmitDropInfoC SNat = forceResetSanity |> fromSignals (mealyB go False)
 where
  go aborted (Nothing, _) = (aborted, (PacketStreamS2M True, Df.NoData))
  go aborted (Just PacketStreamM2S{..}, Ack readyIn) = (nextAborted, (PacketStreamS2M readyIn, fwdOut))
   where
    (nextAborted, fwdOut) = case _last of
      Nothing -> (aborted || _abort, Df.NoData)
      Just i -> (not readyIn && (aborted || _abort), Df.Data (toDropTailInfo i))

    toDropTailInfo i =
      DropTailInfo
        { _dtAborted = aborted || _abort
        , _newIdx = satSub SatWrap i (natToNum @(n `Mod` dataWidth))
        , _drops = drops
        , _wait = wait
        }
     where
      (drops, wait) = case ( compareSNat (SNat @dataWidth) (SNat @n)
                           , sameNat d0 (SNat @(n `Mod` dataWidth))
                           ) of
        (SNatLE, Nothing) ->
          let smaller = (resize i :: Index n) < natToNum @(n - dataWidth)
           in ( if smaller
                  then natToNum @(n `DivRU` dataWidth)
                  else natToNum @(n `Div` dataWidth)
              , not smaller
              )
        (SNatLE, Just Refl) ->
          (natToNum @(n `Div` dataWidth), False)
        (SNatGT, _) ->
          if i >= natToNum @n
            then (0, True)
            else (1, False)

{- |
Gets a delayed packet stream as input together with non-delayed
'DropTailInfo', so that dropping can be done while correctly preserving
'_abort' and adjusting '_last'.
-}
dropTailC' ::
  forall (dataWidth :: Nat) (delay :: Nat) (meta :: Type) (dom :: Domain).
  (KnownDomain dom) =>
  (KnownNat dataWidth) =>
  (KnownNat delay) =>
  (HiddenClockResetEnable dom) =>
  Circuit
    (PacketStream dom dataWidth meta, Df dom (DropTailInfo dataWidth delay))
    (PacketStream dom dataWidth meta)
dropTailC' = fromSignals (B.first unbundle . mealyB go (0, Nothing) . B.first bundle)
 where
  go (0, cache) ((fwdIn, infoIn), bwdIn) = (nextStOut, ((bwdOut1, bwdOut2), fwdOut))
   where
    (bwdOut1, bwdOut2)
      | isNothing fwdOut = (PacketStreamS2M True, Ack True)
      | otherwise = (bwdIn, Ack (_ready bwdIn))

    (nextSt, fwdOut) = case (fwdIn, infoIn) of
      (Nothing, _) ->
        ((0, cache), Nothing)
      (Just pkt, Df.NoData) ->
        case cache of
          Nothing ->
            ((0, Nothing), Just pkt)
          Just (aborted, newIdx, delayy) ->
            ((delayy, Nothing), Just pkt{_abort = aborted, _last = Just newIdx})
      (Just pkt, Df.Data inf) ->
        if _wait inf
          then ((0, Just (_dtAborted inf, _newIdx inf, _drops inf)), Just pkt)
          else
            ( (_drops inf, Nothing)
            , Just pkt{_last = Just (_newIdx inf), _abort = _dtAborted inf}
            )

    nextStOut = if isNothing fwdOut || _ready bwdIn then nextSt else (0, cache)
  go (cycles, cache) _ = ((cycles - 1, cache), ((PacketStreamS2M True, Ack True), Nothing))

{- |
Removes the last @n@ bytes from each packet in the packet stream.
If any dropped transfers had '_abort' set, this will be preserved by
setting the '_abort' of an earlier transfer that is not dropped.

__NB__: assumes that packets are longer than @ceil(n / dataWidth)@ transfers.
If this is not the case, the behaviour of this component is undefined.
-}
dropTailC ::
  forall (dataWidth :: Nat) (meta :: Type) (dom :: Domain) (n :: Nat).
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  (1 <= n) =>
  (NFDataX meta) =>
  SNat n ->
  Circuit
    (PacketStream dom dataWidth meta)
    (PacketStream dom dataWidth meta)
dropTailC SNat = case strictlyPositiveDivRu @n @dataWidth of
  Dict ->
    forceResetSanity
      |> circuit
        ( \stream -> do
            [s1, s2] <- fanout -< stream
            delayed <- delayStreamC (SNat @(n `DivRU` dataWidth)) -< s1
            info <- transmitDropInfoC @dataWidth @(n `DivRU` dataWidth) (SNat @n) -< s2
            dropped <- dropTailC' @dataWidth @(n `DivRU` dataWidth) -< (delayed, info)
            zeroOutInvalidBytesC -< dropped
        )
