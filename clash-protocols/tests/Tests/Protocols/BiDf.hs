{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.Protocols.BiDf (tests) where

import Prelude as P
import qualified Data.List as L

-- clash-prelude
import Clash.Prelude as C
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

-- | Test that 'BiDf.fanin' on a single 'BiDf' channel behaves as an identity.
prop_fanin_id :: Property
prop_fanin_id =
  idWithModelSingleDomain @System defExpectOptions gen (\_ _ _ -> id) (exposeClockResetEnable impl)
 where
  gen :: Gen [Int]
  gen = Gen.list (Range.linear 0 10) (Gen.integral (Range.linear 0 100))

  impl
   :: forall dom a. (HiddenClockResetEnable dom, NFDataX a)
   => Circuit (Df dom a) (Df dom a)
  impl = circuit $ \req -> do
    (biDf, resp) <- BiDf.fromDfs -< req
    BiDf.loopback id <| BiDf.fanin @1 -< [biDf]
    idC -< resp

-- | Test that 'BiDf.fanin' on a number of 'BiDf' channels behaves as an
-- identity on each channel.
prop_fanin :: Property
prop_fanin =
  idWithModelSingleDomain @System expectOpts
    (gen @3)
    (\_ _ _ -> id)
    (exposeClockResetEnable impl)
 where
  expectOpts = defExpectOptions

  gen :: forall n. KnownNat n => Gen (Vec n [(Index n, Int)])
  gen = do
    xs <- genVec @Gen @n $ Gen.list (Range.linear 0 10) (Gen.integral (Range.linear 0 100))
    return $ C.zipWith (\i -> fmap (\x -> (i,x))) indicesI xs

  impl
    :: forall n dom a.
      (HiddenClockResetEnable dom, KnownNat n, 1 <= n, NFDataX a)
    => Circuit (Vec n (Df dom a)) (Vec n (Df dom a))
  impl = circuit $ \reqs -> do
    (biDfs, resps) <- unbundleC <| repeatC BiDf.fromDfs -< reqs
    BiDf.loopback id <| BiDf.fanin @n -< biDfs
    idC -< resps

unbundleC :: forall n a b. Circuit (Vec n (a, b)) (Vec n a, Vec n b)
unbundleC = fromSignals $ \(fwd, (bwdA, bwdB)) ->
  let fwdA :: Vec n (Fwd a)
      fwdB :: Vec n (Fwd b)
      (fwdA, fwdB) = Vector.unzip fwd
  in (Vector.zip bwdA bwdB, (fwdA, fwdB))

tests :: TestTree
tests =
  $(testGroupGenerator)
