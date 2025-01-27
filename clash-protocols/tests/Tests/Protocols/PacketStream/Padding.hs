{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Tests.Protocols.PacketStream.Padding (
  tests,
) where

import Clash.Prelude

import qualified Data.List.Extra as L

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import Protocols.Hedgehog
import Protocols.PacketStream
import Protocols.PacketStream.Hedgehog

-- | Pure model of `stripPaddingC`.
stripPaddingModel ::
  forall p.
  (KnownNat p) =>
  [PacketStreamM2S 1 (Unsigned p)] ->
  [PacketStreamM2S 1 (Unsigned p)]
stripPaddingModel packets = L.concatMap go (chunkByPacket packets)
 where
  go packet
    | packetBytes == expectedSize = packet
    | packetBytes > expectedSize =
        case padding of
          [] ->
            -- There are (packetBytes - expectedSize) bytes, so more than 0
            error "stripPaddingModel: absurd"
          (padding0 : _) ->
            x L.++ [padding0{_last = Just 0, _abort = any _abort padding}]
    | otherwise = a L.++ [b{_abort = True}]
   where
    (a, b) = case L.unsnoc packet of
      Nothing -> error "stripPaddingModel: list should not be empty."
      Just (xs, l) -> (xs, l)

    packetBytes = L.length packet - (if _last b == Just 0 then 1 else 0)
    expectedSize = fromIntegral (_meta b)

    (x, padding) = L.splitAt expectedSize packet

{- |
Test `stripPaddingC` with a given @dataWidth@ against a pure model.

We make sure to test integer overflow by making the data type which holds
the expected packet length extra small: @Unsigned 6@.
-}
stripPaddingProperty ::
  forall dataWidth.
  (1 <= dataWidth) =>
  SNat dataWidth ->
  Property
stripPaddingProperty SNat =
  idWithModelSingleDomain
    @System
    defExpectOptions
    (genPackets 1 10 (genValidPacket defPacketOptions Gen.enumBounded (Range.linear 0 20)))
    (exposeClockResetEnable (upConvert . stripPaddingModel @6 . downConvert))
    (exposeClockResetEnable (stripPaddingC @dataWidth id))

prop_strip_padding_d1 :: Property
prop_strip_padding_d1 = stripPaddingProperty d1

prop_strip_padding_d2 :: Property
prop_strip_padding_d2 = stripPaddingProperty d2

prop_strip_padding_d4 :: Property
prop_strip_padding_d4 = stripPaddingProperty d4

prop_strip_padding_d5 :: Property
prop_strip_padding_d5 = stripPaddingProperty d5

prop_strip_padding_d8 :: Property
prop_strip_padding_d8 = stripPaddingProperty d8

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000 {- 20 seconds -})
    $ localOption
      (HedgehogTestLimit (Just 1_000))
      $(testGroupGenerator)
