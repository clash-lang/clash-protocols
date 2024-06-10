{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Tests.Protocols.Avalon where

-- base
import Prelude

-- clash-prelude
import qualified Clash.Prelude as C

-- extra
import Data.Proxy (Proxy (..))

-- hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen

-- tasty
import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

-- clash-protocols (me!)
import Protocols
import Protocols.Avalon.MemMap
import Protocols.Avalon.Stream
import qualified Protocols.Df as Df
import qualified Protocols.DfConv as DfConv
import Protocols.Hedgehog
import Protocols.Internal

-- tests

import qualified Tests.Protocols.Df as DfTest
import Util

---------------------------------------------------------------
---------------------------- TESTS ----------------------------
---------------------------------------------------------------

type SharedConfig =
  'AvalonMmSharedConfig 2 'True 'True 2 'True 'True 2 'True 2 'True 'True 'True
type ManagerConfig =
  'AvalonMmManagerConfig 'False 'False 'False SharedConfig
type SubordinateConfig =
  'AvalonMmSubordinateConfig
    'True
    'True
    'True
    'False
    'True
    'False
    'False
    'False
    'False
    SharedConfig

genWriteImpt :: Gen (AvalonWriteImpt 'True SharedConfig)
genWriteImpt =
  AvalonWriteImpt
    <$> (toKeepType <$> Gen.enumBounded)
    <*> (toKeepType <$> Gen.enumBounded)
    <*> (toKeepType <$> Gen.enumBounded)
    <*> pure (toKeepType 1)

genReadReqImpt :: Gen (AvalonReadReqImpt 'True SharedConfig)
genReadReqImpt =
  AvalonReadReqImpt
    <$> (toKeepType <$> Gen.enumBounded)
    <*> (toKeepType <$> Gen.enumBounded)
    <*> pure (toKeepType 1)

genReadImpt :: Gen (AvalonReadImpt SharedConfig)
genReadImpt =
  AvalonReadImpt
    <$> (toKeepType <$> Gen.enumBounded)
    <*> (toKeepType <$> Gen.enumBounded)

readReqImpt :: AvalonReadReqImpt 'True SharedConfig
readReqImpt =
  AvalonReadReqImpt
    { rri_addr = toKeepType 0
    , rri_byteEnable = toKeepType 0
    , rri_burstCount = toKeepType 1
    }

readImpt :: AvalonReadImpt SharedConfig
readImpt =
  AvalonReadImpt
    { ri_readData = toKeepType 0
    , ri_endOfPacket = toKeepType False
    }

-- feed ReadImpt's to a manager-to-subordinate converter, and see that the fwd
-- data is preserved
prop_avalon_convert_manager_subordinate :: Property
prop_avalon_convert_manager_subordinate =
  DfTest.idWithModelDf
    defExpectOptions
    (DfTest.genData $ (Left <$> genReadReqImpt) C.<|> (Right <$> genWriteImpt))
    id
    ( C.withClockResetEnable @C.System C.clockGen C.resetGen C.enableGen $
        DfConv.dfConvTestBench
          Proxy
          Proxy
          (repeat True)
          (repeat (Df.Data readImpt))
          ckt
    )
 where
  ckt ::
    (C.HiddenClockResetEnable dom) =>
    Circuit
      (AvalonMmManager dom ManagerConfig)
      (AvalonMmSubordinate dom 0 SubordinateConfig)
  ckt = DfConv.convert Proxy Proxy

-- feed ReadReqImpt's to a manager-to-subordinate converter, and see that the
-- bwd data is preserved
prop_avalon_convert_manager_subordinate_rev :: Property
prop_avalon_convert_manager_subordinate_rev =
  DfTest.idWithModelDf
    defExpectOptions
    (DfTest.genData genReadImpt)
    id
    ( C.withClockResetEnable @C.System C.clockGen C.resetGen C.enableGen $
        DfConv.dfConvTestBenchRev
          Proxy
          Proxy
          (repeat (Df.Data $ Left readReqImpt))
          (repeat True)
          ckt
    )
 where
  ckt ::
    (C.HiddenClockResetEnable dom) =>
    Circuit
      (AvalonMmManager dom ManagerConfig)
      (AvalonMmSubordinate dom 0 SubordinateConfig)
  ckt = DfConv.convert Proxy Proxy

-- feed ReadImpt's to a subordinate-to-manager converter, and see that the fwd
-- data is preserved
prop_avalon_convert_subordinate_manager :: Property
prop_avalon_convert_subordinate_manager =
  DfTest.idWithModelDf
    defExpectOptions
    (DfTest.genData $ (Left <$> genReadReqImpt) C.<|> (Right <$> genWriteImpt))
    id
    ( C.withClockResetEnable @C.System C.clockGen C.resetGen C.enableGen $
        DfConv.dfConvTestBench
          Proxy
          Proxy
          (repeat True)
          (repeat (Df.Data readImpt))
          ckt
    )
 where
  ckt ::
    (C.HiddenClockResetEnable dom) =>
    Circuit
      (AvalonMmSubordinate dom 0 SubordinateConfig)
      (AvalonMmManager dom ManagerConfig)
  ckt = DfConv.convert Proxy Proxy

-- feed ReadReqImpt's to a subordinate-to-manager converter, and see that the
-- bwd data is preserved
prop_avalon_convert_subordinate_manager_rev :: Property
prop_avalon_convert_subordinate_manager_rev =
  DfTest.idWithModelDf
    defExpectOptions
    (DfTest.genData genReadImpt)
    id
    ( C.withClockResetEnable @C.System C.clockGen C.resetGen C.enableGen $
        DfConv.dfConvTestBenchRev
          Proxy
          Proxy
          (repeat (Df.Data $ Left readReqImpt))
          (repeat True)
          ckt
    )
 where
  ckt ::
    (C.HiddenClockResetEnable dom) =>
    Circuit
      (AvalonMmSubordinate dom 0 SubordinateConfig)
      (AvalonMmManager dom ManagerConfig)
  ckt = DfConv.convert Proxy Proxy

-- also test out the DfConv instance for AvalonStream

prop_avalon_stream_fifo_id :: Property
prop_avalon_stream_fifo_id =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (DfTest.genData genInfo)
    (C.exposeClockResetEnable id)
    (C.exposeClockResetEnable @C.System ckt)
    (\a b -> tally a === tally b)
 where
  ckt ::
    (C.HiddenClockResetEnable dom) =>
    Circuit
      (AvalonStream dom ('AvalonStreamConfig 2 2 'True 'True 2 0) Int)
      (AvalonStream dom ('AvalonStreamConfig 2 2 'True 'True 2 0) Int)
  ckt = DfConv.fifo Proxy Proxy (C.SNat @10)

  genInfo =
    AvalonStreamM2S
      <$> DfTest.genSmallInt
      <*> Gen.enumBounded
      <*> Gen.enumBounded
      <*> (toKeepType <$> Gen.enumBounded)
      <*> (toKeepType <$> Gen.enumBounded)
      <*> Gen.enumBounded

tests :: TestTree
tests =
  -- TODO: Move timeout option to hedgehog for better error messages.
  -- TODO: Does not seem to work for combinatorial loops like @let x = x in x@??
  localOption (mkTimeout 12_000_000 {- 12 seconds -}) $
    localOption
      (HedgehogTestLimit (Just 1000))
      $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
