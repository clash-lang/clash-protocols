{-# LANGUAGE NumericUnderscores #-}

module Tests.Haxioms where

import Numeric.Natural
import Prelude

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

{- | Generate a 'Natural' greater than or equal to /n/. Can generate 'Natural's
up to /n+1000/. This should be enough, given that naturals in this module are
used in proofs.
-}
genNatural :: Natural -> Gen Natural
genNatural min_ = Gen.integral (Range.linear min_ (1000 + min_))

-- | Like 'DivRU', but at term-level.
divRU :: Natural -> Natural -> Natural
divRU dividend divider =
  case dividend `divMod` divider of
    (n, 0) -> n
    (n, _) -> n + 1

{- | Test whether the following equation holds:

    DivRU (a * b) b ~ a

Given:

    1 <= b

Tests: 'Data.Constraint.Nat.Extra.cancelMulDiv'.
-}
prop_cancelMulDiv :: Property
prop_cancelMulDiv = property $ do
  a <- forAll (genNatural 0)
  b <- forAll (genNatural 1)
  divRU (a * b) b === a

{- | Test whether the following equation holds:

     Mod a b + 1 <= b

Given:

     1 <= b

Tests: 'Data.Constraint.Nat.Extra.leModulusDivisor'.
-}
prop_leModulusDivisor :: Property
prop_leModulusDivisor = property $ do
  a <- forAll (genNatural 0)
  b <- forAll (genNatural 1)
  assert (a `mod` b + 1 <= b)

{- | Test whether the following equation holds:

    1 <= DivRU a b

Given:

    1 <= a, 1 <= b

Tests: 'Data.Constraint.Nat.Extra.strictlyPositiveDivRu'.
-}
prop_strictlyPositiveDivRu :: Property
prop_strictlyPositiveDivRu = property $ do
  a <- forAll (genNatural 1)
  b <- forAll (genNatural 1)
  assert (1 <= divRU a b)

{- | Test whether the following equation holds:

     b <= a * DivRU b a

Given:

     1 <= a

Tests: 'Data.Constraint.Nat.Extra.leTimesDivRu'.
-}
prop_leTimesDivRu :: Property
prop_leTimesDivRu = property $ do
  a <- forAll (genNatural 1)
  b <- forAll (genNatural 0)
  assert (b <= a * divRU b a)

{- | Test whether the following equation holds:

     a * DivRU b a ~ b + Mod (a - Mod b a) a

Given:

     1 <= a

Tests: 'Data.Constraint.Nat.Extra.eqTimesDivRu'.
-}
prop_eqTimesDivRu :: Property
prop_eqTimesDivRu = property $ do
  a <- forAll (genNatural 1)
  b <- forAll (genNatural 0)
  a * (b `divRU` a) === b + (a - b `mod` a) `mod` a

tests :: TestTree
tests =
  localOption (mkTimeout 10_000_000 {- 10 seconds -}) $
    localOption
      (HedgehogTestLimit (Just 100_000))
      $(testGroupGenerator)
