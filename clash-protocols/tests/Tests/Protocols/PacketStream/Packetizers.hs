{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}

module Tests.Protocols.PacketStream.Packetizers (
  tests,
) where

import Clash.Hedgehog.Sized.Vector (genVec)
import Clash.Prelude

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import Protocols
import qualified Protocols.Df as Df
import Protocols.Hedgehog
import Protocols.PacketStream (packetizeFromDfC, packetizerC)
import Protocols.PacketStream.Base
import Protocols.PacketStream.Hedgehog

{- | Test the packetizer with varying datawidth and number of bytes in the header,
  with metaOut = ().
-}
packetizerPropertyGenerator ::
  forall
    (dataWidth :: Nat)
    (headerBytes :: Nat).
  (1 <= dataWidth) =>
  (1 <= headerBytes) =>
  SNat dataWidth ->
  SNat headerBytes ->
  Property
packetizerPropertyGenerator SNat SNat =
  idWithModelSingleDomain
    @System
    defExpectOptions
    (genValidPackets (Range.linear 1 10) (Range.linear 1 10) Abort)
    (exposeClockResetEnable model)
    (exposeClockResetEnable ckt)
 where
  model = packetize (const ()) id
  ckt ::
    (HiddenClockResetEnable System) =>
    Circuit
      (PacketStream System dataWidth (Vec headerBytes (BitVector 8)))
      (PacketStream System dataWidth ())
  ckt = packetizerC (const ()) id

-- | headerBytes % dataWidth ~ 0
prop_const_packetizer_d1_d14 :: Property
prop_const_packetizer_d1_d14 = packetizerPropertyGenerator d1 d14

-- | dataWidth < headerBytes
prop_const_packetizer_d3_d11 :: Property
prop_const_packetizer_d3_d11 = packetizerPropertyGenerator d3 d11

-- | dataWidth ~ header byte size
prop_const_packetizer_d7_d7 :: Property
prop_const_packetizer_d7_d7 = packetizerPropertyGenerator d7 d7

-- | dataWidth > header byte size
prop_const_packetizer_d5_d4 :: Property
prop_const_packetizer_d5_d4 = packetizerPropertyGenerator d5 d4

{- | Test packetizeFromDf with varying datawidth and number of bytes in the header
  , with metaOut = ().
-}
packetizeFromDfPropertyGenerator ::
  forall
    (dataWidth :: Nat)
    (headerBytes :: Nat).
  (1 <= dataWidth) =>
  (1 <= headerBytes) =>
  SNat dataWidth ->
  SNat headerBytes ->
  Property
packetizeFromDfPropertyGenerator SNat SNat =
  idWithModelSingleDomain
    @System
    defExpectOptions
    (Gen.list (Range.linear 1 10) (genVec Gen.enumBounded))
    (exposeClockResetEnable model)
    (exposeClockResetEnable ckt)
 where
  model = packetizeFromDf (const ()) id
  ckt ::
    (HiddenClockResetEnable System) =>
    Circuit (Df.Df System (Vec headerBytes (BitVector 8))) (PacketStream System dataWidth ())
  ckt = packetizeFromDfC (const ()) id

-- | headerBytes % dataWidth ~ 0
prop_const_packetizeFromDf_d1_d14 :: Property
prop_const_packetizeFromDf_d1_d14 = packetizeFromDfPropertyGenerator d1 d14

-- | dataWidth < headerBytes
prop_const_packetizeFromDf_d3_d11 :: Property
prop_const_packetizeFromDf_d3_d11 = packetizeFromDfPropertyGenerator d3 d11

-- | dataWidth ~ header byte size
prop_const_packetizeFromDf_d7_d7 :: Property
prop_const_packetizeFromDf_d7_d7 = packetizeFromDfPropertyGenerator d7 d7

-- | dataWidth > header byte size
prop_const_packetizeFromDf_d5_d4 :: Property
prop_const_packetizeFromDf_d5_d4 = packetizeFromDfPropertyGenerator d5 d4

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000 {- 20 seconds -}) $
    localOption
      (HedgehogTestLimit (Just 1_000))
      $(testGroupGenerator)
