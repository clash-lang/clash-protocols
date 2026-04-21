-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}

module Tests.Protocols.Df.Extra where

import Clash.Prelude

import Hedgehog (Gen, Property, Range)
import Protocols
import Protocols.Hedgehog (
  defExpectOptions,
  eoStopAfterEmpty,
  idWithModelSingleDomain,
  idWithModelSingleDomainT,
 )
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import Clash.Prelude qualified as C
import Data.List qualified as L
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Protocols.Df qualified as Df
import Protocols.Df.Extra qualified as Df

smallInt :: Range Int
smallInt = Range.linear 0 10

genSmallInt :: Gen Int
genSmallInt =
  Gen.frequency
    [ (90, Gen.integral smallInt)
    , (10, Gen.constant (Range.lowerBound 99 smallInt))
    ]

genData :: Gen a -> Gen [a]
genData genA = do
  n <- genSmallInt
  Gen.list (Range.singleton n) genA

-- | Wrapper around 'skid' that discards the Ready signal
skidDropReady ::
  forall dom a.
  (NFDataX a, HiddenClockResetEnable dom) =>
  Circuit (Df dom a) (Df dom a)
skidDropReady = circuit $ \dfIn -> do
  (dfOut, _ready) <- Df.skid -< dfIn
  idC -< dfOut

prop_skid :: Property
prop_skid =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (genData genSmallInt)
    (C.exposeClockResetEnable id)
    (C.exposeClockResetEnable skidDropReady)

prop_bypassFifo :: Property
prop_bypassFifo =
  idWithModelSingleDomain
    @System
    defExpectOptions
    (genData genSmallInt)
    (C.exposeClockResetEnable id)
    (C.exposeClockResetEnable (Df.bypassFifo d1 (Df.fifo d8)))

prop_stallNext :: Property
prop_stallNext = H.property $ do
  stalls <- H.forAll $ Gen.list (Range.linear 0 100) Gen.bool
  idWithModelSingleDomainT
    @System
    defExpectOptions{eoStopAfterEmpty = Just 150}
    (genData genSmallInt)
    (\_ _ _ -> id)
    (C.exposeClockResetEnable (Df.stallNext (fromList (stalls <> L.repeat True))))

tests :: TestTree
tests = $(testGroupGenerator)
