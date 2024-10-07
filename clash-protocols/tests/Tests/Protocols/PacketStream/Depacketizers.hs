{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}

module Tests.Protocols.PacketStream.Depacketizers (
  tests,
) where

import Clash.Prelude

import Hedgehog (Property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))

import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import Protocols
import Protocols.Hedgehog
import Protocols.PacketStream.Base
import Protocols.PacketStream.Depacketizers
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
    (genPackets 1 4 (genValidPacket defPacketOptions (pure ()) (Range.linear 0 30)))
    (exposeClockResetEnable (depacketizerModel const))
    (exposeClockResetEnable ckt)
 where
  ckt ::
    (HiddenClockResetEnable System) =>
    Circuit
      (PacketStream System dataWidth ())
      (PacketStream System dataWidth (Vec headerBytes (BitVector 8)))
  ckt = depacketizerC const

{- |
Test depacketizeToDf with varying datawidth and number of bytes in the header,
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
    (genPackets 1 10 (genValidPacket defPacketOptions (pure ()) (Range.linear 0 20)))
    (exposeClockResetEnable (depacketizeToDfModel const))
    (exposeClockResetEnable ckt)
 where
  ckt ::
    (HiddenClockResetEnable System) =>
    Circuit (PacketStream System dataWidth ()) (Df System (Vec headerBytes (BitVector 8)))
  ckt = depacketizeToDfC const

-- | Test 'dropTailC' with varying data width and bytes to drop.
dropTailTest ::
  forall dataWidth n.
  (KnownNat n) =>
  (1 <= dataWidth) =>
  (1 <= n) =>
  SNat dataWidth ->
  SNat n ->
  Property
dropTailTest SNat n =
  idWithModelSingleDomain
    @System
    defExpectOptions -- (genPackets 1 10 (genValidPacket defPacketOptions Gen.enumBounded (Range.linear 0 10)))
    ( genPackets
        1
        4
        ( genValidPacket
            defPacketOptions
            (Gen.int8 Range.linearBounded)
            (Range.linear (natToNum @(n `DivRU` dataWidth)) 20)
        )
    )
    (exposeClockResetEnable (dropTailModel n))
    (exposeClockResetEnable (dropTailC @dataWidth n))

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

-- | dataWidth < n && dataWidth % n ~ 0
prop_droptail_4_bytes_d1 :: Property
prop_droptail_4_bytes_d1 = dropTailTest d1 d4

prop_droptail_7_bytes_d1 :: Property
prop_droptail_7_bytes_d1 = dropTailTest d1 d7

-- | dataWidth < n && dataWidth % n > 0
prop_droptail_4_bytes_d3 :: Property
prop_droptail_4_bytes_d3 = dropTailTest d3 d4

prop_droptail_7_bytes_d4 :: Property
prop_droptail_7_bytes_d4 = dropTailTest d4 d7

-- | dataWidth ~ n
prop_droptail_4_bytes_d4 :: Property
prop_droptail_4_bytes_d4 = dropTailTest d4 d4

prop_droptail_7_bytes_d7 :: Property
prop_droptail_7_bytes_d7 = dropTailTest d7 d7

-- | dataWidth > n
prop_droptail_4_bytes_d7 :: Property
prop_droptail_4_bytes_d7 = dropTailTest d7 d4

prop_droptail_7_bytes_d12 :: Property
prop_droptail_7_bytes_d12 = dropTailTest d12 d7

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000 {- 20 seconds -}) $
    localOption
      (HedgehogTestLimit (Just 500))
      $(testGroupGenerator)
