{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE MonomorphismRestriction #-}

module Tests.Protocols.Df.Simple where

-- base
import Data.Coerce (coerce)
import Data.Foldable (fold)
import Data.Maybe (catMaybes)
import GHC.Stack (HasCallStack)
import Prelude

-- clash-prelude
import qualified Clash.Prelude as C
import Clash.Prelude (type (<=))

-- extra
import Data.List (transpose)

-- deepseq
import Control.DeepSeq (NFData)

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
import Util

newtype PlusInt = PlusInt Int
  deriving (NFData, C.Generic, C.NFDataX, C.ShowX, Show, Eq)

instance Semigroup PlusInt where
  PlusInt i <> PlusInt j = PlusInt (i + j)

instance Monoid PlusInt where
  mempty = PlusInt 0

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe genA = Gen.choice [Gen.constant Nothing, Just <$> genA]

smallInt :: Range Int
smallInt = Range.linear 0 10

genSmallInt :: Gen Int
genSmallInt = Gen.integral smallInt

genSmallPlusInt :: Gen PlusInt
genSmallPlusInt = coerce <$> genSmallInt

genData :: Gen a -> Gen [a]
genData genA = do
  n <- genSmallInt
  dat <- Gen.list (Range.singleton n) genA
  pure dat

genVecData :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n [a])
genVecData genA = do
  n <- genSmallInt
  dat <- genVec (Gen.list (Range.singleton n) genA)
  pure dat

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
  Gen [a] ->
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

prop_roundrobinCollectSkip :: Property
prop_roundrobinCollectSkip =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (genVecData genSmallInt)
    (C.exposeClockResetEnable (concat . transpose . C.toList))
    (C.exposeClockResetEnable @C.System (Dfs.roundrobinCollect @3 Dfs.Skip))
    prop
 where
  prop :: [Int] -> [Int] -> PropertyT IO ()
  prop expected actual = tally expected === tally actual

prop_roundrobinCollectParallel :: Property
prop_roundrobinCollectParallel =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (genVecData genSmallInt)
    (C.exposeClockResetEnable (concat . transpose . C.toList))
    (C.exposeClockResetEnable @C.System (Dfs.roundrobinCollect @3 Dfs.Parallel))
    prop
 where
  prop :: [Int] -> [Int] -> PropertyT IO ()
  prop expected actual = tally expected === tally actual

prop_unbundleVec :: Property
prop_unbundleVec =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (fmap C.repeat <$> genData genSmallInt)
    (C.exposeClockResetEnable (vecFromList . transpose . map C.toList))
    (C.exposeClockResetEnable (Dfs.unbundleVec @3 @C.System @Int))

prop_bundleVec :: Property
prop_bundleVec =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (C.repeat <$> genData genSmallPlusInt)
    (C.exposeClockResetEnable (map vecFromList . transpose . C.toList))
    (C.exposeClockResetEnable (Dfs.bundleVec @3 @C.System @PlusInt))

prop_fanin :: Property
prop_fanin =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (genVecData genSmallPlusInt)
    (C.exposeClockResetEnable (map fold . transpose . C.toList))
    (C.exposeClockResetEnable (Dfs.fanin @3 @C.System @PlusInt))

tests :: TestTree
tests =
    -- TODO: Move timeout option to hedgehog for better error messages.
    -- TODO: Does not seem to work for combinatorial loops like @let x = x in x@??
    localOption (mkTimeout 120_000_000 {- 120 seconds -})
  $ localOption (HedgehogTestLimit (Just 1000))
  $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
