{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=5 #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
Copyright  :  (C) 2024, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

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

import Clash.Sized.Vector.Extra (takeLe)
import Data.Constraint (Dict (Dict))
import Data.Constraint.Nat.Extra (leModulusDivisor, strictlyPositiveDivRu)
import Data.Maybe
import Data.Maybe.Extra
import Data.Type.Equality ((:~:) (Refl))

type PacketizerCt (header :: Type) (headerBytes :: Nat) (dataWidth :: Nat) =
  ( BitPack header
  , BitSize header ~ headerBytes * 8
  , KnownNat headerBytes
  , KnownNat dataWidth
  , 1 <= headerBytes
  , 1 <= dataWidth
  )

data PacketizerState1 (metaOut :: Type) (headerBytes :: Nat) (dataWidth :: Nat)
  = Insert1
      { _aborted1 :: Bool
      }
  | Forward1
      { _aborted1 :: Bool
      , _hdrBuf1 :: Vec headerBytes (BitVector 8)
      }
  | LastForward1
      { _aborted1 :: Bool
      , _hdrBuf1 :: Vec headerBytes (BitVector 8)
      }
  deriving (Generic, NFDataX, Show, ShowX)

-- | Packetizer transition function in case @dataWidth > headerBytes@
packetizerT1 ::
  forall
    (headerBytes :: Nat)
    (dataWidth :: Nat)
    (header :: Type)
    (metaIn :: Type)
    (metaOut :: Type).
  (PacketizerCt header headerBytes dataWidth) =>
  (headerBytes + 1 <= dataWidth) =>
  (metaIn -> metaOut) ->
  (metaIn -> header) ->
  PacketizerState1 metaOut headerBytes dataWidth ->
  (Maybe (PacketStreamM2S dataWidth metaIn), PacketStreamS2M) ->
  ( PacketizerState1 metaOut headerBytes dataWidth
  , (PacketStreamS2M, Maybe (PacketStreamM2S dataWidth metaOut))
  )
packetizerT1 toMetaOut toHeader st (Just inPkt, bwdIn) =
  let
    go buf = (nextStOut, (bwdOut, Just outPkt))
     where
      bytesOut :: Vec (dataWidth - headerBytes) (BitVector 8)
      (bytesOut, newBuf) =
        leToPlus @headerBytes @dataWidth $ splitAt (SNat @(dataWidth - headerBytes)) (_data inPkt)
      nextAborted = _aborted1 st || _abort inPkt

      outPkt =
        inPkt
          { _data = buf ++ bytesOut
          , _last = newLast
          , _meta = toMetaOut (_meta inPkt)
          , _abort = nextAborted
          }

      (nextSt, newLast) = case _last inPkt of
        Nothing -> (Forward1 nextAborted newBuf, Nothing)
        Just i
          | i <= natToNum @(dataWidth - headerBytes) ->
              (Insert1 False, Just (i + natToNum @headerBytes))
          | otherwise -> (LastForward1 nextAborted newBuf, Nothing)

      nextStOut = if _ready bwdIn then nextSt else st
      bwdOut = case nextStOut of
        LastForward1{} -> PacketStreamS2M False
        _ -> bwdIn
   in
    case st of
      Insert1{} -> go (bitCoerce (toHeader (_meta inPkt)))
      Forward1{..} -> go _hdrBuf1
      LastForward1{..} -> (nextStOut, (bwdIn, Just outPkt))
       where
        outPkt =
          inPkt
            { _data = _hdrBuf1 ++ repeat (nullByte "packetizerT1")
            , _last = (\i -> i - natToNum @(dataWidth - headerBytes)) <$> _last inPkt
            , _meta = toMetaOut (_meta inPkt)
            , _abort = _aborted1 || _abort inPkt
            }
        nextStOut = if _ready bwdIn then Insert1 False else st
packetizerT1 _ _ s (Nothing, bwdIn) = (s, (bwdIn, Nothing))

data PacketizerState2 (metaOut :: Type) (headerBytes :: Nat) (dataWidth :: Nat)
  = LoadHeader2
  | Insert2
      { _aborted2 :: Bool
      , _hdrBuf2 :: Vec headerBytes (BitVector 8)
      , _counter2 :: Index (headerBytes `DivRU` dataWidth)
      }
  | Forward2
      { _aborted2 :: Bool
      }
  deriving (Generic, NFDataX, Show, ShowX)

-- | Packetizer transition function in case @dataWidth <= headerBytes@ and @headerBytes % dataWidth ~ 0@
packetizerT2 ::
  forall
    (headerBytes :: Nat)
    (dataWidth :: Nat)
    (header :: Type)
    (metaIn :: Type)
    (metaOut :: Type).
  (PacketizerCt header headerBytes dataWidth) =>
  (headerBytes `Mod` dataWidth ~ 0) =>
  (dataWidth <= headerBytes) =>
  (metaIn -> metaOut) ->
  (metaIn -> header) ->
  PacketizerState2 metaOut headerBytes dataWidth ->
  (Maybe (PacketStreamM2S dataWidth metaIn), PacketStreamS2M) ->
  ( PacketizerState2 metaOut headerBytes dataWidth
  , (PacketStreamS2M, Maybe (PacketStreamM2S dataWidth metaOut))
  )
-- Load the metadata into a buffer. This costs one extra cycle of latency, but it reduces resource usage.
packetizerT2 _ toHeader LoadHeader2 (Just inPkt, _) =
  (Insert2 False (bitCoerce (toHeader (_meta inPkt))) 0, (PacketStreamS2M False, Nothing))
packetizerT2 toMetaOut _ st@Insert2{..} (Just inPkt, bwdIn) =
  (nextStOut, (PacketStreamS2M False, Just outPkt))
 where
  (newBuf, dataOut) = leToPlus @dataWidth @headerBytes $ shiftOutFrom0 (SNat @dataWidth) _hdrBuf2
  nextAborted = _aborted2 || _abort inPkt
  outPkt =
    inPkt
      { _data = dataOut
      , _last = Nothing
      , _meta = toMetaOut (_meta inPkt)
      , _abort = nextAborted
      }

  nextSt
    | _counter2 == maxBound = Forward2 nextAborted
    | otherwise = Insert2 nextAborted newBuf (succ _counter2)

  nextStOut = if _ready bwdIn then nextSt else st
packetizerT2 toMetaOut _ Forward2{..} (Just inPkt, bwdIn) =
  (nextStOut, (bwdIn, Just outPkt))
 where
  nextAborted = _aborted2 || _abort inPkt
  outPkt =
    inPkt
      { _meta = toMetaOut (_meta inPkt)
      , _abort = nextAborted
      }
  nextStOut
    | isJust (_last inPkt) && _ready bwdIn = LoadHeader2
    | otherwise = Forward2 nextAborted
packetizerT2 _ _ s (Nothing, bwdIn) = (s, (bwdIn, Nothing))

data PacketizerState3 (headerBytes :: Nat) (dataWidth :: Nat)
  = LoadHeader3
  | Insert3
      { _aborted3 :: Bool
      , _hdrBuf3 :: Vec (headerBytes + dataWidth) (BitVector 8)
      , _counter3 :: Index (headerBytes `DivRU` dataWidth)
      }
  | Forward3
      { _aborted3 :: Bool
      , _hdrBuf3 :: Vec (headerBytes + dataWidth) (BitVector 8)
      }
  | LastForward3
      { _aborted3 :: Bool
      , _hdrBuf3 :: Vec (headerBytes + dataWidth) (BitVector 8)
      }
  deriving (Generic, Show, ShowX)

deriving instance
  (KnownNat headerBytes, KnownNat dataWidth) =>
  NFDataX (PacketizerState3 headerBytes dataWidth)

-- | Packetizer transition function in case @dataWidth <= headerBytes@ and @headerBytes % dataWidth > 0@
packetizerT3 ::
  forall
    (headerBytes :: Nat)
    (dataWidth :: Nat)
    (header :: Type)
    (metaIn :: Type)
    (metaOut :: Type).
  (PacketizerCt header headerBytes dataWidth) =>
  (1 <= headerBytes `Mod` dataWidth) =>
  (headerBytes `Mod` dataWidth <= dataWidth) =>
  (dataWidth <= headerBytes) =>
  (metaIn -> metaOut) ->
  (metaIn -> header) ->
  PacketizerState3 headerBytes dataWidth ->
  (Maybe (PacketStreamM2S dataWidth metaIn), PacketStreamS2M) ->
  ( PacketizerState3 headerBytes dataWidth
  , (PacketStreamS2M, Maybe (PacketStreamM2S dataWidth metaOut))
  )
packetizerT3 _ toHeader LoadHeader3 (Just inPkt, _bwdIn) =
  (nextStOut, (PacketStreamS2M False, Nothing))
 where
  nextStOut = Insert3 False (bitCoerce (toHeader (_meta inPkt)) ++ _data inPkt) 0
packetizerT3 toMetaOut _ st@Insert3{..} (Just inPkt, bwdIn) =
  (nextStOut, (bwdOut, Just outPkt))
 where
  nextAborted = _aborted3 || _abort inPkt
  (newHdrBuf, dataOut') = shiftOutFrom0 (SNat @dataWidth) _hdrBuf3

  outPkt =
    inPkt
      { _data = dataOut'
      , _last = lastOut
      , _meta = toMetaOut (_meta inPkt)
      }

  (lastOut, nextSt) = case (_counter3 == maxBound, _last inPkt) of
    (False, _) -> (Nothing, Insert3 nextAborted newHdrBuf (succ _counter3))
    (True, Nothing) -> (Nothing, Forward3 nextAborted newHdrBuf)
    (True, Just i) ->
      if i <= natToNum @(dataWidth - headerBytes `Mod` dataWidth)
        then (Just (i + natToNum @(headerBytes `Mod` dataWidth)), LoadHeader3)
        else (Nothing, LastForward3 nextAborted newHdrBuf)
  nextStOut = if _ready bwdIn then nextSt else st
  bwdOut = case nextSt of
    LoadHeader3 -> bwdIn
    Forward3{} -> bwdIn
    _ -> PacketStreamS2M False
packetizerT3 toMetaOut _ st@Forward3{..} (Just inPkt, bwdIn) =
  (nextStOut, (bwdOut, Just outPkt))
 where
  bytesOut :: Vec (dataWidth - headerBytes `Mod` dataWidth) (BitVector 8)
  buf :: Vec (headerBytes `Mod` dataWidth) (BitVector 8)
  (bytesOut, buf) = splitAt (SNat @(dataWidth - headerBytes `Mod` dataWidth)) (_data inPkt)
  newBuf :: Vec (headerBytes + dataWidth) (BitVector 8)
  newBuf =
    buf
      ++ repeat @(headerBytes + dataWidth - headerBytes `Mod` dataWidth) (nullByte "packetizerT3")
  nextAborted = _aborted3 || _abort inPkt

  outPkt =
    inPkt
      { _data = take (SNat @(headerBytes `Mod` dataWidth)) _hdrBuf3 ++ bytesOut
      , _last = lastOut
      , _meta = toMetaOut (_meta inPkt)
      , _abort = nextAborted
      }

  (lastOut, nextSt) = case _last inPkt of
    Nothing -> (Nothing, Forward3 nextAborted newBuf)
    Just i ->
      if i <= natToNum @(dataWidth - headerBytes `Mod` dataWidth)
        then (Just (i + natToNum @(headerBytes `Mod` dataWidth)), LoadHeader3)
        else (Nothing, LastForward3 nextAborted newBuf)
  nextStOut = if _ready bwdIn then nextSt else st

  bwdOut = case nextSt of
    LastForward3{} -> PacketStreamS2M False
    _ -> bwdIn
packetizerT3 toMetaOut _ st@LastForward3{..} (Just inPkt, bwdIn) =
  (nextStOut, (bwdIn, Just outPkt))
 where
  outPkt =
    inPkt
      { _data = takeLe (SNat @dataWidth) _hdrBuf3
      , _last = (\i -> i - natToNum @(dataWidth - headerBytes `Mod` dataWidth)) <$> _last inPkt
      , _meta = toMetaOut (_meta inPkt)
      , _abort = _aborted3 || _abort inPkt
      }
  nextStOut = if _ready bwdIn then LoadHeader3 else st
packetizerT3 _ _ s (Nothing, bwdIn) = (s, (bwdIn, Nothing))

{- |
Writes a portion of the metadata to the front of the packet stream, and shifts
the stream accordingly. This portion is defined by the @(metaIn -> header)@
input function. If this function is `id`, the entire metadata is put in front
of the packet stream.
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
  (1 <= headerBytes) =>
  (KnownNat dataWidth) =>
  -- | Mapping from input `_meta` to output `_meta`
  (metaIn -> metaOut) ->
  -- | Mapping from input `_meta` to the header that will be packetized
  (metaIn -> header) ->
  Circuit (PacketStream dom dataWidth metaIn) (PacketStream dom dataWidth metaOut)
packetizerC toMetaOut toHeader = fromSignals outCircuit
 where
  outCircuit = case leModulusDivisor @headerBytes @dataWidth of
    Dict -> case compareSNat (SNat @(headerBytes + 1)) (SNat @dataWidth) of
      SNatLE -> mealyB (packetizerT1 @headerBytes toMetaOut toHeader) (Insert1 False)
      SNatGT ->
        case ( sameNat (SNat @(headerBytes `Mod` dataWidth)) d0
             , compareSNat d1 (SNat @(headerBytes `Mod` dataWidth))
             ) of
          (Just Refl, _) -> mealyB (packetizerT2 @headerBytes toMetaOut toHeader) LoadHeader2
          (Nothing, SNatLE) -> mealyB (packetizerT3 @headerBytes toMetaOut toHeader) LoadHeader3
          (_, _) ->
            clashCompileError
              "packetizerC: unreachable. Report this at https://github.com/clash-lang/clash-protocols/issues"

data DfPacketizerState (metaOut :: Type) (headerBytes :: Nat) (dataWidth :: Nat)
  = DfIdle
  | DfInsert
      { _dfCounter :: Index (headerBytes `DivRU` dataWidth - 1)
      , _dfHdrBuf :: Vec (headerBytes - dataWidth) (BitVector 8)
      }
  deriving (Generic, Show, ShowX)

deriving instance
  (dataWidth <= headerBytes, KnownNat headerBytes, KnownNat dataWidth) =>
  NFDataX (DfPacketizerState metaOut headerBytes dataWidth)

-- | packetizeFromDf state transition function in case dataWidth < headerBytes.
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
  ((dataWidth + 1) <= headerBytes) =>
  -- | Mapping from `Df` input to output `_meta`
  (a -> metaOut) ->
  -- | Mapping from `Df` input to the header that will be packetized
  (a -> header) ->
  DfPacketizerState metaOut headerBytes dataWidth ->
  (Df.Data a, PacketStreamS2M) ->
  ( DfPacketizerState metaOut headerBytes dataWidth
  , (Ack, Maybe (PacketStreamM2S dataWidth metaOut))
  )
packetizeFromDfT toMetaOut toHeader DfIdle (Df.Data dataIn, bwdIn) = (nextStOut, (Ack False, Just outPkt))
 where
  (dataOut, hdrBuf) = splitAt (SNat @dataWidth) (bitCoerce (toHeader dataIn))
  outPkt = PacketStreamM2S dataOut Nothing (toMetaOut dataIn) False
  nextStOut = if _ready bwdIn then DfInsert 0 hdrBuf else DfIdle

-- fwdIn is always Data in this state, because we assert backpressure in Idle before we go here
-- Thus, we don't need to store the metadata in the state.
packetizeFromDfT toMetaOut _ st@DfInsert{..} (Df.Data dataIn, bwdIn) = (nextStOut, (bwdOut, Just outPkt))
 where
  (dataOut, newHdrBuf) =
    splitAt (SNat @dataWidth) (_dfHdrBuf ++ repeat @dataWidth (nullByte "packetizeFromDfT"))
  outPkt = PacketStreamM2S dataOut newLast (toMetaOut dataIn) False

  newLast = toMaybe (_dfCounter == maxBound) $ case compareSNat (SNat @(headerBytes `Mod` dataWidth)) d0 of
    SNatGT -> natToNum @(headerBytes `Mod` dataWidth)
    _ -> natToNum @dataWidth

  bwdOut = Ack (_ready bwdIn && _dfCounter == maxBound)
  nextSt = if _dfCounter == maxBound then DfIdle else DfInsert (succ _dfCounter) newHdrBuf
  nextStOut = if _ready bwdIn then nextSt else st
packetizeFromDfT _ _ s (Df.NoData, bwdIn) = (s, (Ack (_ready bwdIn), Nothing))

{- |
Starts a packet stream upon receiving some data over a `Df` channel.
The bytes to be packetized and the output metadata are specified by the
input functions.
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
  (1 <= headerBytes) =>
  (1 <= dataWidth) =>
  -- | Mapping from `Df` input to output `_meta`
  (a -> metaOut) ->
  -- | Mapping from `Df` input to the header that will be packetized
  (a -> header) ->
  Circuit (Df dom a) (PacketStream dom dataWidth metaOut)
packetizeFromDfC toMetaOut toHeader = case strictlyPositiveDivRu @headerBytes @dataWidth of
  Dict -> case compareSNat (SNat @headerBytes) (SNat @dataWidth) of
    -- We don't need a state machine in this case, as we are able to packetize
    -- the entire payload in one clock cycle.
    SNatLE -> Circuit (unbundle . fmap go . bundle)
     where
      go (Df.NoData, _) = (Ack False, Nothing)
      go (Df.Data dataIn, bwdIn) = (Ack (_ready bwdIn), Just outPkt)
       where
        outPkt = PacketStreamM2S dataOut (Just l) (toMetaOut dataIn) False
        dataOut =
          bitCoerce (toHeader dataIn)
            ++ repeat @(dataWidth - headerBytes) (nullByte "packetizeFromDfC")
        l = case compareSNat (SNat @(headerBytes `Mod` dataWidth)) d0 of
          SNatGT -> natToNum @(headerBytes `Mod` dataWidth)
          _ -> natToNum @dataWidth
    SNatGT -> fromSignals (mealyB (packetizeFromDfT toMetaOut toHeader) DfIdle)
