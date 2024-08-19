{-# LANGUAGE NumericUnderscores #-}
-- TODO: Fix warnings introduced by GHC 9.2 w.r.t. incomplete lazy pattern matches
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Tests.Protocols.DfConv where

-- base

import Data.Maybe (fromMaybe)
import Prelude

-- clash-prelude
import qualified Clash.Prelude as C

-- list
import Data.List (mapAccumL, partition, transpose)

-- containers
import qualified Data.HashMap.Strict as HashMap

-- extra
import Data.Proxy (Proxy (..))

-- hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- tasty
import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

-- clash-protocols (me!)
import Protocols
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

-- test a small selection of dflike functions on df
-- this is moreso to test @instance DfConv Df@,
-- as well as @dfToDfConvInp@ etc,
-- rather than the functions themselves,
-- since we know they work from @Tests.Protocols.Df@

prop_df_map_inc :: Property
prop_df_map_inc =
  DfTest.idWithModelDf'
    (fmap (+ 1))
    (C.withClockResetEnable C.clockGen C.resetGen C.enableGen ckt)
 where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit (Df dom Int) (Df dom Int)
  ckt = DfConv.map Proxy Proxy (+ 1)

prop_df_filter_over_5 :: Property
prop_df_filter_over_5 =
  DfTest.idWithModelDf'
    (filter (> 5))
    (C.withClockResetEnable C.clockGen C.resetGen C.enableGen ckt)
 where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit (Df dom Int) (Df dom Int)
  ckt = DfConv.filter Proxy Proxy (> 5)

prop_df_mapmaybe_inc_over_5 :: Property
prop_df_mapmaybe_inc_over_5 =
  DfTest.idWithModelDf'
    (map (+ 1) . filter (> 5))
    (C.withClockResetEnable C.clockGen C.resetGen C.enableGen ckt)
 where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit (Df dom Int) (Df dom Int)
  ckt = DfConv.mapMaybe Proxy Proxy (\n -> if n > 5 then Just (n + 1) else Nothing)

prop_df_zipwith_add :: Property
prop_df_zipwith_add =
  idWithModel
    defExpectOptions
    ( do
        as <- DfTest.genData DfTest.genSmallInt
        bs <- DfTest.genData DfTest.genSmallInt
        let n = min (length as) (length bs)
        pure (take n as, take n bs)
    )
    (uncurry (zipWith (+)))
    (C.withClockResetEnable @C.System C.clockGen C.resetGen C.enableGen ckt)
 where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit (Df dom Int, Df dom Int) (Df dom Int)
  ckt = DfConv.zipWith (Proxy, Proxy) Proxy (+)

prop_df_fanout1 :: Property
prop_df_fanout1 =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (DfTest.genData DfTest.genSmallInt)
    (C.exposeClockResetEnable C.repeat)
    (C.exposeClockResetEnable ckt)
 where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit (Df dom Int) (C.Vec 1 (Df dom Int))
  ckt = DfConv.fanout Proxy Proxy

prop_df_fanout2 :: Property
prop_df_fanout2 =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (DfTest.genData DfTest.genSmallInt)
    (C.exposeClockResetEnable C.repeat)
    (C.exposeClockResetEnable ckt)
 where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit (Df dom Int) (C.Vec 2 (Df dom Int))
  ckt = DfConv.fanout Proxy Proxy

prop_df_fanout7 :: Property
prop_df_fanout7 =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (DfTest.genData DfTest.genSmallInt)
    (C.exposeClockResetEnable C.repeat)
    (C.exposeClockResetEnable ckt)
 where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit (Df dom Int) (C.Vec 7 (Df dom Int))
  ckt = DfConv.fanout Proxy Proxy

prop_df_partition :: Property
prop_df_partition =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (DfTest.genData DfTest.genSmallInt)
    (C.exposeClockResetEnable $ partition (> 5))
    (C.exposeClockResetEnable ckt)
 where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit (Df dom Int) (Df dom Int, Df dom Int)
  ckt = DfConv.partition Proxy (Proxy, Proxy) (> 5)

prop_df_fanin :: Property
prop_df_fanin =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (DfTest.genVecData DfTest.genSmallInt)
    (C.exposeClockResetEnable $ map sum . transpose . C.toList)
    (C.exposeClockResetEnable ckt)
 where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit (C.Vec 3 (Df dom Int)) (Df dom Int)
  ckt = DfConv.fanin Proxy Proxy (+)

prop_df_fifo_id :: Property
prop_df_fifo_id =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (DfTest.genData DfTest.genSmallInt)
    (C.exposeClockResetEnable id)
    (C.exposeClockResetEnable @C.System ckt)
    (\a b -> tally a === tally b)
 where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit (Df dom Int) (Df dom Int)
  ckt = DfConv.fifo Proxy Proxy (C.SNat @10)

prop_select :: Property
prop_select =
  idWithModel
    defExpectOptions
    goGen
    (snd . uncurry (mapAccumL goModel))
    (C.withClockResetEnable @C.System C.clockGen C.resetGen C.enableGen ckt)
 where
  ckt ::
    (C.HiddenClockResetEnable dom) =>
    Circuit (C.Vec 3 (Df dom Int), Df dom (C.Index 3)) (Df dom Int)
  ckt = DfConv.select (Proxy @(Df _ Int), (Proxy @(Df _ (C.Index 3)))) (Proxy @(Df _ Int))

  goModel :: C.Vec 3 [Int] -> C.Index 3 -> (C.Vec 3 [Int], Int)
  goModel vec ix = let (i : is) = vec C.!! ix in (C.replace ix is vec, i)

  goGen :: Gen (C.Vec 3 [Int], [C.Index 3])
  goGen = do
    n <- DfTest.genSmallInt
    ixs <- Gen.list (Range.singleton n) Gen.enumBounded
    let tall i = fromMaybe 0 (HashMap.lookup i (tally ixs))
    dats <- mapM (\i -> Gen.list (Range.singleton (tall i)) DfTest.genSmallInt) C.indicesI
    pure (dats, ixs)

-- test out instance DfConv (Reverse a)

prop_reverse_df_convert_id :: Property
prop_reverse_df_convert_id =
  DfTest.idWithModelDf'
    id
    (C.withClockResetEnable C.clockGen C.resetGen C.enableGen ckt)
 where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit (Df dom Int) (Df dom Int)
  ckt =
    coerceCircuit $
      reverseCircuit $
        DfConv.convert (Proxy @(Reverse (Df _ _))) (Proxy @(Reverse (Df _ _)))

-- test out the test bench
prop_test_bench_id :: Property
prop_test_bench_id =
  DfTest.idWithModelDf
    defExpectOptions
    (DfTest.genData DfTest.genSmallInt)
    id
    ( C.withClockResetEnable @C.System C.clockGen C.resetGen C.enableGen $
        DfConv.dfConvTestBench
          Proxy
          Proxy
          (repeat True)
          (repeat (Df.Data 0))
          ckt
    )
 where
  ckt ::
    (C.HiddenClockResetEnable dom) =>
    Circuit
      (Df dom Int, Reverse (Df dom Int))
      (Df dom Int, Reverse (Df dom Int))
  ckt = DfConv.convert Proxy Proxy

prop_test_bench_rev_id :: Property
prop_test_bench_rev_id =
  DfTest.idWithModelDf
    defExpectOptions
    (DfTest.genData DfTest.genSmallInt)
    id
    ( C.withClockResetEnable @C.System C.clockGen C.resetGen C.enableGen $
        DfConv.dfConvTestBenchRev
          Proxy
          Proxy
          (repeat (Df.Data 0))
          (repeat True)
          ckt
    )
 where
  ckt ::
    (C.HiddenClockResetEnable dom) =>
    Circuit
      (Df dom Int, Reverse (Df dom Int))
      (Df dom Int, Reverse (Df dom Int))
  ckt = DfConv.convert Proxy Proxy

tests :: TestTree
tests =
  -- TODO: Move timeout option to hedgehog for better error messages.
  -- TODO: Does not seem to work for combinatorial loops like @let x = x in x@??
  localOption (mkTimeout 20_000_000 {- 20 seconds -}) $
    localOption
      (HedgehogTestLimit (Just 1000))
      $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
