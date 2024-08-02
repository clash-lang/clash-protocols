{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}

module Tests.Protocols.PacketStream.Delay where

-- base
import Prelude

-- clash-prelude
import qualified Clash.Prelude as C

-- hedgehog
import Hedgehog
import qualified Hedgehog.Range as Range

-- tasty
import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

-- clash-protocols
import Protocols
import Protocols.Hedgehog
import Protocols.PacketStream

-- tests
import Tests.Protocols.PacketStream.Base

prop_delaystream_id :: Property
prop_delaystream_id =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (genValidPackets (Range.linear 1 10) (Range.linear 1 6) Abort)
    (C.exposeClockResetEnable id)
    (C.exposeClockResetEnable ckt)
 where
  ckt ::
    (C.HiddenClockResetEnable C.System) =>
    Circuit (PacketStream C.System 2 ()) (PacketStream C.System 2 ())
  ckt = delayStream @C.System @2 @() @4 C.d4

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000 {- 20 seconds -}) $
    localOption
      (HedgehogTestLimit (Just 1_000))
      $(testGroupGenerator)
