{-# LANGUAGE CPP #-}
-- TODO: Fix warnings introduced by GHC 9.2 w.r.t. incomplete lazy pattern matches
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-- Hashable (Index n)
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tests.Protocols.Df where

-- base
import Data.Bifunctor (Bifunctor (first))
import Data.Coerce (coerce)
import Data.Foldable (fold)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Tuple (swap)
import GHC.Stack (HasCallStack)
import Prelude
#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (Applicative(liftA2))
#endif

-- clash-prelude

import Clash.Explicit.Prelude qualified as E
import Clash.Explicit.Reset (noReset)
import Clash.Prelude (Vec (Nil, (:>)), type (<=))
import Clash.Prelude qualified as C

-- containers
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet

-- extra
import Data.Either (partitionEithers)
import Data.List (mapAccumL, partition, transpose)

-- deepseq
import Control.DeepSeq (NFData)

-- hashable
import Data.Hashable (Hashable)

-- hedgehog
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

-- tasty
import Test.Tasty
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

-- clash-protocols (me!)
import Protocols
import Protocols.Df qualified as Df
import Protocols.Hedgehog

-- tests
import Util

newtype PlusInt = PlusInt Int
  deriving stock (C.Generic, Show)
  deriving anyclass (C.ShowX)
  deriving newtype (NFData, C.NFDataX, Eq)

instance Semigroup PlusInt where
  (<>) :: PlusInt -> PlusInt -> PlusInt
  PlusInt i <> PlusInt j = PlusInt (i + j)

instance Monoid PlusInt where
  mempty = PlusInt 0

instance Hashable (C.Index n)

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe genA = Gen.choice [Gen.constant Nothing, Just <$> genA]

smallInt :: Range Int
smallInt = Range.linear 0 10

genSmallInt :: Gen Int
genSmallInt =
  Gen.frequency
    [ (90, Gen.integral smallInt)
    , (10, Gen.constant (Range.lowerBound 99 smallInt))
    ]

genSmallPlusInt :: Gen PlusInt
genSmallPlusInt = coerce <$> genSmallInt

genData :: Gen a -> Gen [a]
genData genA = do
  n <- genSmallInt
  Gen.list (Range.singleton n) genA

genVecData :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n [a])
genVecData genA = do
  n <- genSmallInt
  genVec (Gen.list (Range.singleton n) genA)

-- Same as 'idWithModel', but specialized on 'Df'
idWithModelDf ::
  forall a b.
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
prop_filter = idWithModelDf' (filter (> 5)) (Df.filter (> 5))

prop_catMaybes :: Property
prop_catMaybes =
  idWithModelDf
    defExpectOptions
    (genData (genMaybe genSmallInt))
    catMaybes
    Df.catMaybes

-- A parameterized test definition validating that an expander which
-- simply releases a value downstream once every N cycles
-- does not otherwise change the contents of the stream.
testExpanderPassThrough :: forall n. (C.KnownNat n) => C.SNat n -> Property
testExpanderPassThrough _periodicity =
  idWithModelSingleDomain @C.System
    defExpectOptions
    (genData genSmallInt)
    (C.exposeClockResetEnable id)
    ( C.exposeClockResetEnable $
        passThroughExpander |> Df.catMaybes
    )
 where
  -- Just stares at a value for a few cycles and then forwards it
  passThroughExpander ::
    forall dom a.
    (C.HiddenClockResetEnable dom) =>
    Circuit (Df dom a) (Df dom (Maybe a))
  passThroughExpander = Df.expander (0 :: C.Index n) $ \count input ->
    let done = count == maxBound
     in ( if done then 0 else count + 1
        , if done then Just input else Nothing
        , done
        )

prop_expander_passthrough_linerate :: Property
prop_expander_passthrough_linerate = testExpanderPassThrough C.d1

prop_expander_passthrough_slow :: Property
prop_expander_passthrough_slow = testExpanderPassThrough C.d4

-- A parameterized test definition validating that an expander duplicates
-- input values N times and sends them downstream.
testExpanderDuplicate :: forall n. (C.KnownNat n) => C.SNat n -> Property
testExpanderDuplicate duplication =
  idWithModelSingleDomain @C.System
    defExpectOptions
    (genData genSmallInt)
    (C.exposeClockResetEnable (concatMap (replicate (C.snatToNum duplication))))
    ( C.exposeClockResetEnable
        duplicator
    )
 where
  -- Creates n copies of a value
  duplicator ::
    forall dom a.
    (C.HiddenClockResetEnable dom) =>
    Circuit (Df dom a) (Df dom a)
  duplicator = Df.expander (0 :: C.Index n) $ \count input ->
    let done = count == maxBound
     in ( if done then 0 else count + 1
        , input
        , done
        )

prop_expander_duplicate_linerate :: Property
prop_expander_duplicate_linerate = testExpanderDuplicate C.d1

prop_expander_duplicate_slow :: Property
prop_expander_duplicate_slow = testExpanderDuplicate C.d4

-- A paremterized test definition validating that a compressor correctly
-- sums up batches of N values.
testCompressorSum :: forall n. (C.KnownNat n) => C.SNat n -> Property
testCompressorSum batchSize =
  idWithModelSingleDomain @C.System
    defExpectOptions
    (genData genSmallInt)
    (C.exposeClockResetEnable referenceImpl)
    ( C.exposeClockResetEnable
        passThroughExpander
    )
 where
  chunk = C.snatToNum batchSize
  -- Given [a,b,c,d,e] and chunk = 2, yield [a+b,c+d]
  referenceImpl = map sum . takeWhile ((== chunk) . length) . map (take chunk) . iterate (drop chunk)
  -- Sum groups of `n` samples together
  passThroughExpander ::
    forall dom.
    (C.HiddenClockResetEnable dom) =>
    Circuit (Df dom Int) (Df dom Int)
  passThroughExpander = Df.compressor (0 :: C.Index n, 0 :: Int) $ \(count, total) input ->
    let done = count == maxBound
        total' = total + input
     in ( if done then (0, 0) else (count + 1, total')
        , if done then Just total' else Nothing
        )

prop_compressor_sum_linerate :: Property
prop_compressor_sum_linerate = testCompressorSum C.d1

prop_compressor_sum_slow :: Property
prop_compressor_sum_slow = testCompressorSum C.d4

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
  prop expected actual = HashSet.fromList expected === HashSet.fromList actual

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
  prop expected actual = HashSet.fromList expected === HashSet.fromList actual

{- | Asserts that roundrobinCollect with Parallel mode behaves in a left-biased
fashion.
-}
case_roundrobinCollectParallel :: Assertion
case_roundrobinCollectParallel = do
  expected @?= actual
 where
  actual =
    E.sampleN 5
      . C.bundle
      . first C.bundle
      $ dut (input0 :> input1 :> input2 :> Nil, pure $ Ack True)

  expected =
    [ (Ack True :> Ack False :> Ack False :> Nil, Just (1 :: Int))
    , (Ack True :> Ack False :> Ack False :> Nil, Just 2)
    , (Ack False :> Ack True :> Ack False :> Nil, Just 10)
    , (Ack False :> Ack True :> Ack False :> Nil, Just 40)
    , (Ack False :> Ack False :> Ack True :> Nil, Just 100)
    ]

  input0 = C.fromList @_ @C.System [Just 1, Just 2, Nothing, Nothing, Nothing]
  input1 = C.fromList @_ @C.System [Just 10, Just 10, Just 10, Just 40, Nothing]
  input2 = C.fromList @_ @C.System [Just 100, Just 100, Just 100, Just 100, Just 100]

  dut =
    toSignals $
      C.withClockResetEnable @C.System C.clockGen noReset C.enableGen $
        Df.roundrobinCollect @3 Df.Parallel

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
        pure (take n as, take n bs)
    )
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
        pure (take n as, take n bs)
    )
    (uncurry zip)
    (Df.zip @Int @Int @C.System)

prop_partition :: Property
prop_partition =
  idWithModel
    defExpectOptions
    (genData genSmallInt)
    (partition (> 5))
    (Df.partition @C.System @Int (> 5))

prop_partitionEithers :: Property
prop_partitionEithers =
  idWithModel
    defExpectOptions
    (genData (Gen.either genSmallInt Gen.alphaNum))
    partitionEithers
    (Df.partitionEithers @C.System @Int @Char)

prop_route :: Property
prop_route =
  idWithModel
    defExpectOptions
    (zip <$> genData Gen.enumBounded <*> genData genSmallInt)
    (\inp -> C.map (\i -> map snd (filter ((== i) . fst) inp)) C.indicesI)
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
  goModel vec ix = let (i : is) = vec C.!! ix in (C.replace ix is vec, i)

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
    let (as, bs) = splitAt (fromIntegral len) (vec C.!! ix)
     in (C.replace ix bs vec, as)

  goGen :: Gen (C.Vec 3 [Int], [(C.Index 3, C.Index 10)])
  goGen = do
    n <- genSmallInt
    ixs <- Gen.list (Range.singleton n) Gen.enumBounded
    lenghts <- Gen.list (Range.singleton n) Gen.enumBounded
    let tallied = tallyOn fst (fromIntegral . snd) (zip ixs lenghts)
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
    let (as, (b : bs)) = break snd (vec C.!! ix)
     in (C.replace ix bs vec, as <> [b])

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

prop_fifo :: Property
prop_fifo =
  idWithModelDf'
    id
    (C.withClockResetEnable C.clockGen C.resetGen C.enableGen Df.fifo (C.SNat @10))

prop_toMaybeFromMaybe :: Property
prop_toMaybeFromMaybe =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (genData genSmallInt)
    (\_ _ _ -> fmap (,0))
    (C.exposeClockResetEnable dut)
    (\sent received -> prop (fst <$> sent) received)
 where
  -- Calculate how many samples were dropped and use that to validate the received
  -- stream.
  prop :: [Int] -> [(Int, C.Unsigned 64)] -> PropertyT IO ()
  prop sents (unzip -> (receiveds, nDroppeds)) = do
    footnote ("sents: " <> show sents)
    footnote ("receiveds: " <> show receiveds)
    footnote ("nDroppeds: " <> show nDroppeds)
    footnote ("nDroppedSinceLasts: " <> show nDroppedSinceLasts)
    go sents (zip receiveds nDroppedSinceLasts)
   where
    nDroppedSinceLasts :: [C.Unsigned 64]
    nDroppedSinceLasts = 0 : zipWith (-) nDroppeds (0 : nDroppeds)

    go _ [] = pure ()
    go sent0 ((received, nDropped) : rest)
      | (s : ss) <- drop (fromIntegral nDropped) sent0 = do
          s === received
          go ss rest
      | otherwise = fail "Expected more sent values"

  -- XXX: This dut is a bit *meh*, because it inserts the dropped count into
  --      the Df stream, but it doesn't account for the rule that the data
  --      only "advances" when data is acked.
  dut ::
    (C.SystemClockResetEnable) =>
    Circuit
      (Df C.System Int)
      (Df C.System (Int, C.Unsigned 64))
  dut =
    Df.forceResetSanity
      |> Df.toMaybe
      |> Df.unsafeFromMaybe
      |> Circuit Proxy Proxy (first (,()) . swap . first (fmap go) . first C.bundle)
   where
    go :: (Maybe Int, C.Unsigned 64) -> Maybe (Int, C.Unsigned 64)
    go (a, b) = liftA2 (,) a (Just b)

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
