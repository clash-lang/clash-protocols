{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}

module Tests.Protocols.PacketStream.Depacketizers (
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
import Protocols.PacketStream (depacketizeToDfC, depacketizerC)
import Protocols.PacketStream.Base
import Protocols.PacketStream.Hedgehog

{- | Test the depacketizer with varying datawidth and number of bytes in the header,
  with metaIn = () and toMetaOut = const.
-}
depacketizerPropertyGenerator ::
  forall
    (dataWidth :: Nat)
    (headerBytes :: Nat).
  (1 <= dataWidth) =>
  (1 <= headerBytes) =>
  SNat dataWidth ->
  SNat headerBytes ->
  Property
depacketizerPropertyGenerator SNat SNat =
  idWithModelSingleDomain
    @System
    defExpectOptions{eoSampleMax = 1000, eoStopAfterEmpty = 1000}
    (genPackets (Range.linear 1 4) Abort (genValidPacket (pure ()) (Range.linear 1 30)))
    (exposeClockResetEnable model)
    (exposeClockResetEnable ckt)
 where
  model = depacketizerModel const
  ckt ::
    (HiddenClockResetEnable System) =>
    Circuit
      (PacketStream System dataWidth ())
      (PacketStream System dataWidth (Vec headerBytes (BitVector 8)))
  ckt = depacketizerC const

-- | headerBytes % dataWidth ~ 0
prop_const_depacketizer_d1_d14 :: Property
prop_const_depacketizer_d1_d14 = depacketizerPropertyGenerator d1 d14

-- | dataWidth < headerBytes
prop_const_depacketizer_d3_d11 :: Property
prop_const_depacketizer_d3_d11 = depacketizerPropertyGenerator d3 d11

-- | dataWidth ~ header byte size
prop_const_depacketizer_d7_d7 :: Property
prop_const_depacketizer_d7_d7 = depacketizerPropertyGenerator d7 d7

-- | dataWidth > header byte size
prop_const_depacketizer_d5_d4 :: Property
prop_const_depacketizer_d5_d4 = depacketizerPropertyGenerator d5 d4

{- | Test depacketizeToDf with varying datawidth and number of bytes in the header,
  with metaIn = () and toMetaOut = const.
-}
depacketizeToDfPropertyGenerator ::
  forall
    (dataWidth :: Nat)
    (headerBytes :: Nat).
  (1 <= dataWidth) =>
  (1 <= headerBytes) =>
  SNat dataWidth ->
  SNat headerBytes ->
  Property
depacketizeToDfPropertyGenerator SNat SNat =
  idWithModelSingleDomain
    @System
    defExpectOptions{eoSampleMax = 1000, eoStopAfterEmpty = 1000}
    (genPackets (Range.linear 1 10) Abort (genValidPacket (pure ()) (Range.linear 1 20)))
    (exposeClockResetEnable model)
    (exposeClockResetEnable ckt)
 where
  model = depacketizeToDfModel const
  ckt ::
    (HiddenClockResetEnable System) =>
    Circuit (PacketStream System dataWidth ()) (Df System (Vec headerBytes (BitVector 8)))
  ckt = depacketizeToDfC const

-- | headerBytes % dataWidth ~ 0
prop_const_depacketize_to_df_d1_d14 :: Property
prop_const_depacketize_to_df_d1_d14 = depacketizeToDfPropertyGenerator d1 d14

-- | dataWidth < headerBytes
prop_const_depacketize_to_df_d3_d11 :: Property
prop_const_depacketize_to_df_d3_d11 = depacketizeToDfPropertyGenerator d3 d11

-- | dataWidth ~ header byte size
prop_const_depacketize_to_df_d7_d7 :: Property
prop_const_depacketize_to_df_d7_d7 = depacketizeToDfPropertyGenerator d7 d7

-- | dataWidth > header byte size
prop_const_depacketize_to_df_d5_d4 :: Property
prop_const_depacketize_to_df_d5_d4 = depacketizeToDfPropertyGenerator d5 d4

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000 {- 20 seconds -}) $
    localOption
      (HedgehogTestLimit (Just 100))
      $(testGroupGenerator)
