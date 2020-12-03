{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE MonomorphismRestriction #-}

module Tests.Protocols.Df.Simple (tests, main) where

-- base
import Data.Maybe (catMaybes)
import GHC.Stack (HasCallStack)
import Prelude

-- clash-prelude
import qualified Clash.Prelude as C

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
import Protocols.Df.Simple (Dfs)
import qualified Protocols.Df.Simple as Dfs
import Protocols.Hedgehog

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe genA = Gen.choice [Gen.constant Nothing, Just <$> genA]

smallInt :: Range Int
smallInt = Range.linear 0 10

genSmallInt :: Gen Int
genSmallInt = Gen.integral smallInt

genData :: Gen a -> Gen ([a], Int)
genData genA = do
  n <- genSmallInt
  dat <- Gen.list (Range.singleton n) genA
  pure (dat, n)

-- Same as 'idWithModel', but specialized on 'Df'
idWithModelDfs ::
  forall a b .
  (HasCallStack, TestType a, TestType b) =>
  -- | Options, see 'ExpectOptions'
  ExpectOptions ->
  -- | Test data generator, length of generated data is number of _valid_
  -- cycles. If an input consists of multiple input channels where the number
  -- of valid cycles differs, this should return the _maximum_ number of valid
  -- cycles of all channels.
  Gen ([a], Int) ->
  -- | Model
  ([a] -> [b]) ->
  -- | Implementation
  Circuit (Dfs C.System a) (Dfs C.System b) ->
  Property
idWithModelDfs = idWithModel

-- | Same as 'idWithModelDfs' but with hardcoded data generator
idWithModelDfs' ::
  -- | Model
  ([Int] -> [Int]) ->
  -- | Implementation
  Circuit (Dfs C.System Int) (Dfs C.System Int) ->
  Property
idWithModelDfs' = idWithModelDfs defExpectOptions (genData genSmallInt)

---------------------------------------------------------------
---------------------------- TESTS ----------------------------
---------------------------------------------------------------
prop_id :: Property
prop_id = idWithModelDfs' id idC

prop_map :: Property
prop_map = idWithModelDfs' (map succ) (Dfs.map succ)

prop_filter :: Property
prop_filter = idWithModelDfs' (filter (>5)) (Dfs.filter (>5))

prop_catMaybes :: Property
prop_catMaybes =
  idWithModelDfs
    defExpectOptions
    (genData (genMaybe genSmallInt))
    catMaybes
    Dfs.catMaybes

prop_registerFwd :: Property
prop_registerFwd =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (genData genSmallInt)
    (C.exposeClockResetEnable id)
    (C.exposeClockResetEnable Dfs.registerFwd)

prop_registerBwd :: Property
prop_registerBwd =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (genData genSmallInt)
    (C.exposeClockResetEnable id)
    (C.exposeClockResetEnable Dfs.registerBwd)

prop_fanout1 :: Property
prop_fanout1 =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (genData genSmallInt)
    (C.exposeClockResetEnable C.repeat)
    (C.exposeClockResetEnable @C.System (Dfs.fanout @1))

prop_fanout2 :: Property
prop_fanout2 =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (genData genSmallInt)
    (C.exposeClockResetEnable C.repeat)
    (C.exposeClockResetEnable @C.System (Dfs.fanout @2))

prop_fanout7 :: Property
prop_fanout7 =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (genData genSmallInt)
    (C.exposeClockResetEnable C.repeat)
    (C.exposeClockResetEnable @C.System (Dfs.fanout @7))

tests :: TestTree
tests =
    -- TODO: Move timeout option to hedgehog for better error messages.
    -- TODO: Does not seem to work for combinatorial loops like @let x = x in x@??
    localOption (mkTimeout 120_000_000 {- 120 seconds -})
  $ localOption (HedgehogTestLimit (Just 1000))
  $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
