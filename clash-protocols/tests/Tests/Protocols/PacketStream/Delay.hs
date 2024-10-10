{-# LANGUAGE NumericUnderscores #-}

module Tests.Protocols.PacketStream.Delay (
  tests,
) where

import Clash.Prelude

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

prop_delaystream_id :: Property
prop_delaystream_id =
  idWithModelSingleDomain
    @System
    defExpectOptions
    (genPackets 1 10 (genValidPacket defPacketOptions Gen.enumBounded (Range.linear 4 20)))
    (exposeClockResetEnable id)
    (exposeClockResetEnable (delayStreamC @2 @Int d4))

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000 {- 20 seconds -}) $
    localOption
      (HedgehogTestLimit (Just 1_000))
      $(testGroupGenerator)
