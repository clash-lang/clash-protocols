{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE MonomorphismRestriction #-}

module Tests.Protocols.Df.Simple where

-- base
import Data.Maybe (catMaybes)
import GHC.Stack (HasCallStack)
import Prelude

-- clash-prelude
import qualified Clash.Prelude as C
import Clash.Prelude (type (<=))

-- extra
import Data.List (transpose)

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

-- tests
import Util (chunksOf, genVec)

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

prop_roundrobin :: Property
prop_roundrobin =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (genData genSmallInt)
    (C.exposeClockResetEnable chunksOf)
    (C.exposeClockResetEnable @C.System (Dfs.roundrobin @3))

prop_roundrobinCollectNoSkip :: Property
prop_roundrobinCollectNoSkip =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (genVecData genSmallInt)
    (C.exposeClockResetEnable (concat . transpose . C.toList))
    (C.exposeClockResetEnable @C.System (Dfs.roundrobinCollect @3 Dfs.NoSkip))
 where
  genVecData :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n [a], Int)
  genVecData genA = do
    n <- genSmallInt
    dat <- genVec (Gen.list (Range.singleton n) genA)
    pure (dat, n)

-- TODO:
--   We can't currently test the /Skip/ and /Parallel/ modes of
--   'roundRobinCollect', as its output is unstable due to unpredictable
--   stalling on both sides of the circuit. We need to generalize
--   'idWithModelSingleDomain' in order to test for arbitrary properties.
--

tests :: TestTree
tests =
    -- TODO: Move timeout option to hedgehog for better error messages.
    -- TODO: Does not seem to work for combinatorial loops like @let x = x in x@??
    localOption (mkTimeout 120_000_000 {- 120 seconds -})
  $ localOption (HedgehogTestLimit (Just 1000))
  $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
