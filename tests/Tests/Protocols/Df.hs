{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE MonomorphismRestriction #-}

module Tests.Protocols.Df (tests, main) where

-- base
import Data.Maybe (catMaybes)
import GHC.Stack (HasCallStack)
import Prelude

-- clash-prelude
import qualified Clash.Prelude as C

-- extra
import qualified Data.Tuple.Extra as T

-- hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- tasty
import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit(HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

-- clash-protocols (me!)
import Protocols
import qualified Protocols.Df as Df
import Protocols.Hedgehog

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe genA = Gen.choice [Gen.constant Nothing, Just <$> genA]

smallInt :: Range Int
smallInt = Range.linear 0 10

genSmallInt :: Gen Int
genSmallInt = Gen.integral smallInt

genData :: Gen a -> Gen [(Int, a)]
genData genA = do
  n <- genSmallInt
  dat <- Gen.list (Range.singleton n) ((,) <$> genSmallInt <*> genA)
  pure dat

-- Same as 'idWithModel', but specialized on 'Df'
idWithModelDf ::
  forall a b .
  (HasCallStack, TestType a, TestType b) =>
  -- | Options, see 'ExpectOptions'
  ExpectOptions ->
  -- | Test data generator, length of generated data is number of _valid_
  -- cycles. If an input consists of multiple input channels where the number
  -- of valid cycles differs, this should return the _maximum_ number of valid
  -- cycles of all channels.
  Gen [(Int, a)] ->
  -- | Model
  ([(Int, a)] -> [(Int, b)]) ->
  -- | Implementation
  Circuit (Df C.System Int a) (Df C.System Int b) ->
  Property
idWithModelDf = idWithModel

-- | Same as 'idWithModelDf' but with hardcoded data generator
idWithModelDf' ::
  -- | Model
  ([(Int, Int)] -> [(Int, Int)]) ->
  -- | Implementation
  Circuit (Df C.System Int Int) (Df C.System Int Int) ->
  Property
idWithModelDf' = idWithModelDf defExpectOptions (genData genSmallInt)

---------------------------------------------------------------
---------------------------- TESTS ----------------------------
---------------------------------------------------------------
prop_id :: Property
prop_id = idWithModelDf' id idC

prop_map :: Property
prop_map = idWithModelDf' (map (T.second (+1))) (Df.map (+1))

prop_mapMeta :: Property
prop_mapMeta = idWithModelDf' (map (T.first (+1))) (Df.mapMeta (+1))

prop_filter :: Property
prop_filter = idWithModelDf' (filter ((>5) . snd)) (Df.filter (>5))

prop_filterMeta :: Property
prop_filterMeta = idWithModelDf' (filter ((>5) . fst)) (Df.filterMeta (>5))

prop_catMaybes :: Property
prop_catMaybes =
  idWithModelDf
    defExpectOptions
    (genData (genMaybe genSmallInt))
    (catMaybes . map (uncurry (C.liftA2 (,)) . T.first Just))
    Df.catMaybes

prop_registerFwd :: Property
prop_registerFwd =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (genData genSmallInt)
    (C.exposeClockResetEnable id)
    (C.exposeClockResetEnable Df.registerFwd)

prop_registerBwd :: Property
prop_registerBwd =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (genData genSmallInt)
    (C.exposeClockResetEnable id)
    (C.exposeClockResetEnable Df.registerBwd)

prop_fanout1 :: Property
prop_fanout1 =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (genData genSmallInt)
    (C.exposeClockResetEnable C.repeat)
    (C.exposeClockResetEnable @C.System (Df.fanout @1))

prop_fanout2 :: Property
prop_fanout2 =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (genData genSmallInt)
    (C.exposeClockResetEnable C.repeat)
    (C.exposeClockResetEnable @C.System (Df.fanout @2))

prop_fanout7 :: Property
prop_fanout7 =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (genData genSmallInt)
    (C.exposeClockResetEnable C.repeat)
    (C.exposeClockResetEnable @C.System (Df.fanout @7))

tests :: TestTree
tests =
    -- TODO: Move timeout option to hedgehog for better error messages.
    -- TODO: Does not seem to work for combinatorial loops like @let x = x in x@??
    localOption (mkTimeout 120_000_000 {- 120 seconds -})
  $ localOption (HedgehogTestLimit (Just 1000))
  $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
