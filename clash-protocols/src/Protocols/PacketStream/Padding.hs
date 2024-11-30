{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
Copyright   :  (C) 2024, QBayLogic B.V.
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Provides a generic component which enforces some expected packet length field
in the metadata.
-}
module Protocols.PacketStream.Padding (
  stripPaddingC,
) where

import Clash.Prelude

import qualified Data.Bifunctor as B
import Data.Maybe
import Data.Type.Equality ((:~:) (Refl))

import Protocols
import Protocols.PacketStream.Base

-- | State of `stripPaddingT`.
data StripPaddingState p dataWidth meta
  = Counting
      { _buffer :: PacketStreamM2S dataWidth meta
      -- ^ Contains the last transfer, with `_abort` set if a premature end
      --   was detected. If the packet contained padding, `_last` is already
      --   correctly adjusted.
      , _valid :: Bool
      -- ^ Qualifier for _buffer. If false, its value is undefined.
      , _counter :: Unsigned p
      -- ^ Counts the actual length of the current packet.
      }
  | Strip
      { _buffer :: PacketStreamM2S dataWidth meta
      -- ^ We need to wait until forwarding the last transfer, as the padding
      --   may be aborted. In this state we do not need _valid, as the buffered
      --   transfer is always valid.
      }
  deriving (Generic, NFDataX)

-- | State transition function of `stripPaddingC`.
stripPaddingT ::
  forall dataWidth meta p.
  (KnownNat dataWidth) =>
  (KnownNat p) =>
  (meta -> Unsigned p) ->
  StripPaddingState p dataWidth meta ->
  ( Maybe (PacketStreamM2S dataWidth meta)
  , PacketStreamS2M
  ) ->
  ( StripPaddingState p dataWidth meta
  , ( PacketStreamS2M
    , Maybe (PacketStreamM2S dataWidth meta)
    )
  )
stripPaddingT _ st@Counting{} (Nothing, bwdIn) = (nextSt, (PacketStreamS2M True, fwdOut))
 where
  fwdOut =
    if _valid st
      then Just (_buffer st)
      else Nothing

  nextSt
    | isJust fwdOut && _ready bwdIn = st{_valid = False}
    | otherwise = st
stripPaddingT toLength st@Counting{} (Just inPkt, bwdIn) = (nextSt, (bwdOut, fwdOut))
 where
  expectedLen = toLength (_meta inPkt)

  toAdd :: Unsigned p
  toAdd = case _last inPkt of
    Nothing -> natToNum @dataWidth
    -- Here we do a slightly dangerous resize. Because @dataWidth@ should
    -- never be bigger than @2^p@, this is not an issue in practice, so I
    -- don't believe it requires a constraint as long as it is well-documented.
    Just size -> bitCoerce (resize size :: Index (2 ^ p))

  carry :: Bool
  nextCount :: Unsigned p
  (carry, nextCount) =
    B.bimap unpack unpack
      $ split
      $ add (_counter st) toAdd

  -- True if the payload size is smaller than expected.
  -- We have to take the carry into account as well, otherwise if the
  -- calculation overflows then we will wrongly signal a premature end.
  prematureEnd =
    isJust (_last inPkt)
      && (nextCount < expectedLen)
      && not carry

  tooBig = nextCount > expectedLen || carry

  fwdOut =
    if _valid st
      then Just (_buffer st)
      else Nothing

  bwdOut = PacketStreamS2M (isNothing fwdOut || _ready bwdIn)

  nextLast
    -- If @dataWidth is 1, the adjusted `_last` is always @Just 0@.
    -- Otherwise, we need to do some arithmetic.
    | tooBig = case sameNat d1 (SNat @dataWidth) of
        Just Refl -> Just 0
        Nothing -> Just $ bitCoerce $ resize $ expectedLen - _counter st
    | otherwise = _last inPkt

  nextBuf = inPkt{_last = nextLast, _abort = _abort inPkt || prematureEnd}
  nextValid = isJust (_last inPkt) || not tooBig

  nextCounter =
    if prematureEnd || isJust (_last inPkt)
      then 0
      else nextCount

  nextSt
    | isJust fwdOut && not (_ready bwdIn) = st
    | isNothing (_last inPkt) && tooBig = Strip nextBuf
    | otherwise = Counting nextBuf nextValid nextCounter
stripPaddingT _ st@Strip{} (Nothing, _) = (st, (PacketStreamS2M True, Nothing))
stripPaddingT _ Strip{_buffer = f} (Just inPkt, _) =
  (nextSt, (PacketStreamS2M True, Nothing))
 where
  nextAborted = _abort f || _abort inPkt

  nextSt =
    if isJust (_last inPkt)
      then Counting f{_abort = nextAborted} True 0
      else Strip (f{_abort = nextAborted})

{- |
Removes padding from packets according to some expected packet length field
in the metadata. If the actual length of a packet is smaller than expected,
the packet is aborted.

Has one clock cycle of latency, because all M2S outputs are registered.
Runs at full throughput.

__NB__: @dataWidth@ /must/ be smaller than @2^p@. Because this should never
occur in practice, this constraint is not enforced on the type-level.
-}
stripPaddingC ::
  forall dataWidth meta p dom.
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (KnownNat p) =>
  (NFDataX meta) =>
  -- | Function that extracts the expected packet length from the metadata
  (meta -> Unsigned p) ->
  Circuit (PacketStream dom dataWidth meta) (PacketStream dom dataWidth meta)
stripPaddingC toLength =
  forceResetSanity
    |> fromSignals (mealyB (stripPaddingT toLength) s0)
 where
  s0 =
    Counting
      { _buffer = deepErrorX "stripPaddingT: undefined initial buffer."
      , _valid = False
      , _counter = 0
      }
