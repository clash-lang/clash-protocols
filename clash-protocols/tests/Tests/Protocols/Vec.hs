{-# LANGUAGE NumericUnderscores #-}

module Tests.Protocols.Vec where

-- base
import Prelude

-- clash-prelude
import Clash.Prelude (System)
import qualified Clash.Prelude as C

-- hedgehog
import Hedgehog

-- tasty
import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

-- clash-protocols (me!)
import Protocols
import qualified Protocols.Vec as Vec

import Clash.Hedgehog.Sized.Vector (genVec)
import Protocols.Hedgehog

-- tests
import Tests.Protocols.Df (genData, genSmallInt, genVecData)

prop_append :: Property
prop_append =
  idWithModel
    @(C.Vec 2 (Df System Int), C.Vec 3 (Df System Int))
    defExpectOptions
    gen
    model
    dut
 where
  gen =
    (,)
      <$> genVecData genSmallInt
      <*> genVecData genSmallInt
  dut = Vec.append
  model = uncurry (C.++)

prop_append3 :: Property
prop_append3 =
  idWithModel
    @(C.Vec 2 (Df System Int), C.Vec 3 (Df System Int), C.Vec 4 (Df System Int))
    @(C.Vec 9 (Df System Int))
    defExpectOptions
    gen
    model
    dut
 where
  gen :: Gen (C.Vec 2 [Int], C.Vec 3 [Int], C.Vec 4 [Int])
  gen =
    (,,)
      <$> genVecData genSmallInt
      <*> genVecData genSmallInt
      <*> genVecData genSmallInt
  dut = Vec.append3
  model (a, b, c) = (a C.++ b) C.++ c

prop_split :: Property
prop_split =
  idWithModel
    @(C.Vec 5 (Df System Int))
    @(C.Vec 2 (Df System Int), C.Vec 3 (Df System Int))
    defExpectOptions
    gen
    model
    dut
 where
  gen = genVecData genSmallInt
  dut = Vec.split
  model = C.splitAtI

prop_split3 :: Property
prop_split3 =
  idWithModel
    @(C.Vec 9 (Df System Int))
    @(C.Vec 2 (Df System Int), C.Vec 3 (Df System Int), C.Vec 4 (Df System Int))
    defExpectOptions
    gen
    model
    dut
 where
  gen = genVecData genSmallInt
  dut = Vec.split3
  model v = (v0, v1, v2)
   where
    (v0, C.splitAtI -> (v1, v2)) = C.splitAtI v

prop_zip :: Property
prop_zip =
  idWithModel
    @(C.Vec 2 (Df System Int), C.Vec 2 (Df System Int))
    defExpectOptions
    gen
    model
    dut
 where
  gen =
    (,)
      <$> genVecData genSmallInt
      <*> genVecData genSmallInt
  dut = Vec.zip
  model (a, b) = C.zip a b

prop_zip3 :: Property
prop_zip3 =
  idWithModel
    @(C.Vec 2 (Df System Int), C.Vec 2 (Df System Int), C.Vec 2 (Df System Int))
    defExpectOptions
    gen
    model
    dut
 where
  gen =
    (,,)
      <$> genVecData genSmallInt
      <*> genVecData genSmallInt
      <*> genVecData genSmallInt
  dut = Vec.zip3
  model (a, b, c) = C.zip3 a b c

prop_unzip :: Property
prop_unzip =
  idWithModel
    @(C.Vec 2 (Df System Int, Df System Int))
    defExpectOptions
    gen
    model
    dut
 where
  gen = genVec ((,) <$> genData genSmallInt <*> genData genSmallInt)
  dut = Vec.unzip
  model = C.unzip

prop_unzip3 :: Property
prop_unzip3 =
  idWithModel
    @(C.Vec 2 (Df System Int, Df System Int, Df System Int))
    defExpectOptions
    gen
    model
    dut
 where
  gen = genVec ((,,) <$> genData genSmallInt <*> genData genSmallInt <*> genData genSmallInt)
  dut = Vec.unzip3
  model = C.unzip3

prop_concat :: Property
prop_concat =
  idWithModel
    @(C.Vec 2 (C.Vec 3 (Df System Int)))
    defExpectOptions
    gen
    model
    dut
 where
  gen = genVec (genVecData genSmallInt)
  dut = Vec.concat
  model = C.concat

prop_unconcat :: Property
prop_unconcat =
  idWithModel
    @(C.Vec 6 (Df System Int))
    defExpectOptions
    gen
    model
    dut
 where
  gen = genVecData genSmallInt
  dut = Vec.unconcat C.d2
  model = C.unconcat C.d2

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
