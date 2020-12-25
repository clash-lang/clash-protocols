{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE MonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- Hashable (Index n)

module Tests.Protocols.Df where

-- base
import Data.Coerce (coerce)
import Data.Foldable (fold)
import Data.Maybe (catMaybes, fromMaybe)
import Data.List (mapAccumL)
import GHC.Stack (HasCallStack)
import Prelude

-- clash-prelude
import qualified Clash.Prelude as C
import Clash.Prelude (type (<=))

-- containers
import qualified Data.HashMap.Strict as HashMap

-- extra
import Data.List (transpose, partition)

-- deepseq
import Control.DeepSeq (NFData)

-- hashable
import Data.Hashable (Hashable)

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

-- tests
import Util

newtype PlusInt = PlusInt Int
  deriving (NFData, C.Generic, C.NFDataX, C.ShowX, Show, Eq)

instance Semigroup PlusInt where
  PlusInt i <> PlusInt j = PlusInt (i + j)

instance Monoid PlusInt where
  mempty = PlusInt 0

instance Hashable (C.Index n)

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
idWithModelDf ::
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
  Circuit (Df C.System a) (Df C.System b) ->
  Property
idWithModelDf = idWithModel

-- | Same as 'idWithModelDf' but with hardcoded data generator
idWithModelDf' ::
  -- | Model
  ([Int] -> [Int]) ->
  -- | Implementation
  Circuit (Df C.System Int) (Df C.System Int) ->
  Property
idWithModelDf' = idWithModelDf defExpectOptions (genData genSmallInt)

---------------------------------------------------------------
---------------------------- TESTS ----------------------------
---------------------------------------------------------------
prop_id :: Property
prop_id = idWithModelDf' id idC

prop_map :: Property
prop_map = idWithModelDf' (map succ) (Df.map succ)

prop_filter :: Property
prop_filter = idWithModelDf' (filter (>5)) (Df.filter (>5))

prop_catMaybes :: Property
prop_catMaybes =
  idWithModelDf
    defExpectOptions
    (genData (genMaybe genSmallInt))
    catMaybes
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

prop_roundrobin :: Property
prop_roundrobin =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (genData genSmallInt)
    (C.exposeClockResetEnable chunksOf)
    (C.exposeClockResetEnable @C.System (Df.roundrobin @3))

prop_roundrobinCollectNoSkip :: Property
prop_roundrobinCollectNoSkip =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (genVecData genSmallInt)
    (C.exposeClockResetEnable (concat . transpose . C.toList))
    (C.exposeClockResetEnable @C.System (Df.roundrobinCollect @3 Df.NoSkip))

prop_roundrobinCollectSkip :: Property
prop_roundrobinCollectSkip =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (genVecData genSmallInt)
    (C.exposeClockResetEnable (concat . transpose . C.toList))
    (C.exposeClockResetEnable @C.System (Df.roundrobinCollect @3 Df.Skip))
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
    (C.exposeClockResetEnable @C.System (Df.roundrobinCollect @3 Df.Parallel))
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
    (C.exposeClockResetEnable (Df.unbundleVec @3 @C.System @Int))

prop_bundleVec :: Property
prop_bundleVec =
  idWithModel
    defExpectOptions
    (C.repeat <$> genData genSmallPlusInt)
    (map vecFromList . transpose . C.toList)
    (Df.bundleVec @3 @C.System @PlusInt)

prop_fanin :: Property
prop_fanin =
  idWithModel
    defExpectOptions
    (genVecData genSmallInt)
    (map sum . transpose . C.toList)
    (Df.fanin @3 @C.System @Int (+))

prop_mfanin :: Property
prop_mfanin =
  idWithModel
    defExpectOptions
    (genVecData genSmallPlusInt)
    (map fold . transpose . C.toList)
    (Df.mfanin @3 @C.System @PlusInt)

prop_zipWith :: Property
prop_zipWith =
  idWithModel
    defExpectOptions
    ( do
        as <- genData genSmallInt
        bs <- genData genSmallInt
        let n = min (length as) (length bs)
        pure (take n as, take n bs) )
    (uncurry (zipWith (+)))
    (Df.zipWith @C.System @Int @Int (+))

prop_zip :: Property
prop_zip =
  idWithModel
    defExpectOptions
    ( do
        as <- genData genSmallInt
        bs <- genData genSmallInt
        let n = min (length as) (length bs)
        pure (take n as, take n bs) )
    (uncurry zip)
    (Df.zip @Int @Int @C.System)

prop_partition :: Property
prop_partition =
  idWithModel
    defExpectOptions
    (genData genSmallInt)
    (partition (>5))
    (Df.partition @C.System @Int (>5))

prop_route :: Property
prop_route =
  idWithModel
    defExpectOptions
    (zip <$> genData Gen.enumBounded <*> genData genSmallInt)
    (\inp -> C.map (\i -> map snd (filter ((==i) . fst) inp)) C.indicesI)
    (Df.route @3 @C.System @Int)

prop_select :: Property
prop_select =
  idWithModel
    defExpectOptions
    goGen
    (snd . uncurry (mapAccumL goModel))
    (Df.select @3 @C.System @Int)
 where
  goModel :: C.Vec 3 [Int] -> C.Index 3 -> (C.Vec 3 [Int], Int)
  goModel vec ix = let (i:is) = vec C.!! ix in (C.replace ix is vec, i)

  goGen :: Gen (C.Vec 3 [Int], [C.Index 3])
  goGen = do
    n <- genSmallInt
    ixs <- Gen.list (Range.singleton n) Gen.enumBounded
    let tall i = fromMaybe 0 (HashMap.lookup i (tally ixs))
    dats <- mapM (\i -> Gen.list (Range.singleton (tall i)) genSmallInt) C.indicesI
    pure (dats, ixs)

prop_selectN :: Property
prop_selectN =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    goGen
    (\_ _ _ -> concat . snd . uncurry (mapAccumL goModel))
    (C.exposeClockResetEnable (Df.selectN @3 @10 @C.System @Int))
 where
  goModel :: C.Vec 3 [Int] -> (C.Index 3, C.Index 10) -> (C.Vec 3 [Int], [Int])
  goModel vec (ix, len) =
    let (as, bs) = splitAt (fromIntegral len) (vec C.!! ix) in
    (C.replace ix bs vec, as)

  goGen :: Gen (C.Vec 3 [Int], [(C.Index 3, C.Index 10)])
  goGen = do
    n <- genSmallInt
    ixs <- Gen.list (Range.singleton n) Gen.enumBounded
    lenghts <- Gen.list (Range.singleton n) Gen.enumBounded
    let
      tallied = tallyOn fst (fromIntegral . snd) (zip ixs lenghts)
      tall i = fromMaybe 0 (HashMap.lookup i tallied)
    dats <- mapM (\i -> Gen.list (Range.singleton (tall i)) genSmallInt) C.indicesI
    pure (dats, zip ixs lenghts)

prop_selectUntil :: Property
prop_selectUntil =
  idWithModel
    defExpectOptions
    goGen
    (concat . snd . uncurry (mapAccumL goModel))
    (Df.selectUntil @3 @C.System @(Int, Bool) snd)
 where
  goModel :: C.Vec 3 [(Int, Bool)] -> C.Index 3 -> (C.Vec 3 [(Int, Bool)], [(Int, Bool)])
  goModel vec ix =
    let (as, (b:bs)) = break snd (vec C.!! ix) in
    (C.replace ix bs vec, as <> [b])

  goGen :: Gen (C.Vec 3 [(Int, Bool)], [C.Index 3])
  goGen = do
    n <- genSmallInt
    ixs <- Gen.list (Range.singleton n) Gen.enumBounded
    dats <- mapM (\i -> goChannelInput (HashMap.lookup i (tally ixs))) C.indicesI
    pure (dats, ixs)

  goChannelInput :: Maybe Int -> Gen [(Int, Bool)]
  goChannelInput Nothing = pure []
  goChannelInput (Just n) = do
    inputs0 <- Gen.list (Range.singleton n) (Gen.list (Range.linear 1 10) genSmallInt)
    let tagEnd xs = zip (init xs) (repeat False) <> [(last xs, True)]
    pure (concatMap tagEnd inputs0)

tests :: TestTree
tests =
    -- TODO: Move timeout option to hedgehog for better error messages.
    -- TODO: Does not seem to work for combinatorial loops like @let x = x in x@??
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1000))
  $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
