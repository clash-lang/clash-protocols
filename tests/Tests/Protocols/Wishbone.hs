{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Tests.Protocols.Wishbone where

import Clash.Hedgehog.Sized.BitVector
import Clash.Prelude hiding (not, (&&))
-- tasty
import Control.DeepSeq (NFData)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Protocols
import Protocols.Hedgehog (defExpectOptions)
import Protocols.Wishbone
import Protocols.Wishbone.Standard
import Protocols.Wishbone.Standard.Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)
import Prelude hiding (undefined)

smallInt :: Range Int
smallInt = Range.linear 0 10

genSmallInt :: Gen Int
genSmallInt = Gen.integral smallInt

genData :: Gen a -> Gen [a]
genData = Gen.list (Range.linear 0 150)

genWishboneTransfer ::
  (KnownNat addressWidth, KnownNat (BitSize a)) =>
  Gen a ->
  Gen (WishboneMasterRequest addressWidth a)
genWishboneTransfer genA =
  Gen.choice
    [ Read <$> genDefinedBitVector <*> genDefinedBitVector ,
      Write <$> genDefinedBitVector <*> genDefinedBitVector <*> genA
    ]

--
-- 'addrReadId' circuit
--

addrReadIdWb ::
  forall dom addressWidth.
  (KnownNat addressWidth) =>
  Circuit (Wishbone dom 'Standard addressWidth (BitVector addressWidth)) ()
addrReadIdWb = Circuit go
  where
    go (m2s, ()) = (reply <$> m2s, ())

    reply WishboneM2S {..}
      | busCycle && strobe && writeEnable =
        emptyWishboneS2M { acknowledge = True }
      | busCycle && strobe =
        (emptyWishboneS2M @(BitVector addressWidth))
          { acknowledge = True,
            readData = addr
          }
      | otherwise = emptyWishboneS2M

addrReadIdWbModel ::
  (KnownNat addressWidth) =>
  WishboneMasterRequest addressWidth (BitVector addressWidth) ->
  WishboneS2M (BitVector addressWidth) ->
  -- | pure state
  () ->
  Either String ()
addrReadIdWbModel (Read addr _) s@WishboneS2M {..} ()
  | acknowledge && hasX readData == Right addr = Right ()
  | otherwise =
    Left $ "Read should have been acknowledged with address as DAT " <> showX s
addrReadIdWbModel Write {} s@WishboneS2M {..} ()
  | acknowledge && hasUndefined readData = Right ()
  | otherwise =
    Left $ "Write should have been acknowledged with no DAT " <> showX s

prop_addrReadIdWb_model :: Property
prop_addrReadIdWb_model = withClockResetEnable clockGen resetGen enableGen $
  wishbonePropWithModel @System
    defExpectOptions
    addrReadIdWbModel
    addrReadIdWb
    (genData $ genWishboneTransfer @10 genDefinedBitVector)
    ()

--
-- memory element circuit
--

memoryWbModel ::
  ( KnownNat addressWidth,
    Eq a,
    ShowX a,
    NFDataX a,
    NFData a,
    Default a,
    KnownNat (BitSize a)
  ) =>
  WishboneMasterRequest addressWidth a ->
  WishboneS2M a ->
  [(BitVector addressWidth, a)] ->
  Either String [(BitVector addressWidth, a)]
memoryWbModel (Read addr sel) s st
  | sel /= maxBound && err s = Right st
  | sel /= maxBound && not (err s) =
    Left "Read with non maxBound SEL should cause ERR response"
  | otherwise = case lookup addr st of
    Just x | readData s == x && acknowledge s -> Right st
    Just x | otherwise ->
      Left $
        "Read from a known address did not yield the same value "
          <> showX x
          <> " : "
          <> showX s
    Nothing | acknowledge s && hasX (readData s) == Right def -> Right st
    Nothing | otherwise ->
      Left $
        "Read from unknown address did no ACK with undefined result : "
          <> showX s
memoryWbModel (Write addr sel a) s st
  | sel /= maxBound && err s = Right st
  | sel /= maxBound && not (err s) =
    Left "Write with non maxBound SEL should cause ERR response"
  | acknowledge s = Right ((addr, a) : st)
  | otherwise = Left $ "Write should be acked : " <> showX s

prop_memoryWb_model :: Property
prop_memoryWb_model =
  withClockResetEnable clockGen resetGen enableGen $
    wishbonePropWithModel @System
      defExpectOptions
      memoryWbModel
      (memoryWb (blockRamU ClearOnReset (SNat @256) (const def)))
      (genData (genWishboneTransfer @8 genSmallInt))
      [] -- initial state

tests :: TestTree
tests =
  -- TODO: Move timeout option to hedgehog for better error messages.
  -- TODO: Does not seem to work for combinatorial loops like @let x = x in x@??
  localOption (mkTimeout 12_000_000 {- 12 seconds -}) $
    localOption
      (HedgehogTestLimit (Just 1000))
      $(testGroupGenerator)
