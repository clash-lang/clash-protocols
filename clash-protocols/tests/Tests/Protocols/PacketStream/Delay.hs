{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}

module Tests.Protocols.PacketStream.Delay (
  tests,
) where

import Clash.Prelude

import Hedgehog
import qualified Hedgehog.Range as Range

import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import Protocols
import Protocols.Hedgehog
import Protocols.PacketStream
import Protocols.PacketStream.Hedgehog

prop_delaystream_id :: Property
prop_delaystream_id =
  idWithModelSingleDomain
    @System
    defExpectOptions
    (genValidPackets (Range.linear 1 10) (Range.linear 1 6) Abort)
    (exposeClockResetEnable id)
    (exposeClockResetEnable ckt)
 where
  ckt ::
    (HiddenClockResetEnable System) =>
    Circuit (PacketStream System 2 ()) (PacketStream System 2 ())
  ckt = delayStream @System @2 @() @4 d4

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000 {- 20 seconds -}) $
    localOption
      (HedgehogTestLimit (Just 1_000))
      $(testGroupGenerator)
