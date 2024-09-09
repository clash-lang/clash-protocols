{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.Protocols.BiDf (tests) where

-- clash-prelude
import Clash.Prelude
import qualified Clash.Sized.Vector as Vector
import Clash.Hedgehog.Sized.Vector

-- clash-protocols
import Protocols
import Protocols.Hedgehog
import Protocols.BiDf as BiDf

-- hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- tasty
import Test.Tasty
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

-- | Ensure that 'BiDf.toDfs' composed with 'BiDf.fromDfs' behaves as an
-- identity.
prop_toDfs_fromDfs_id :: Property
prop_toDfs_fromDfs_id =
  idWithModelSingleDomain @System defExpectOptions gen (\_ _ _ -> id) (exposeClockResetEnable impl)
 where
  gen :: Gen [Int]
  gen = Gen.list (Range.linear 0 10) (Gen.integral (Range.linear 0 100))

  impl :: forall dom a. (HiddenClockResetEnable dom, NFDataX a)
       => Circuit (Df dom a) (Df dom a)
  impl = BiDf.toDfs <| BiDf.fromDfs

-- | Ensure that 'BiDf.loopback' behaves as an identity.
prop_loopback_id :: Property
prop_loopback_id =
  idWithModelSingleDomain @System defExpectOptions gen (\_ _ _ -> id) (exposeClockResetEnable impl)
 where
  gen :: Gen [Int]
  gen = Gen.list (Range.linear 0 10) (Gen.integral (Range.linear 0 100))

  impl :: forall dom a. (HiddenClockResetEnable dom, NFDataX a)
       => Circuit (Df dom a) (Df dom a)
  impl = circuit $ \req -> do
    (biDf, resp) <- BiDf.fromDfs -< req
    BiDf.loopback id -< biDf
    idC -< resp

tests :: TestTree
tests =
  $(testGroupGenerator)