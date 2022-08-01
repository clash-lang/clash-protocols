{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
-- The exhaustiveness-checker doesn't work well with ViewPatterns
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Tests.Protocols.Wishbone where

import Clash.Hedgehog.Sized.BitVector
import Clash.Prelude hiding ((&&))
-- tasty
import Control.DeepSeq (NFData)
import Data.Either (isLeft)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Protocols
import Protocols.Hedgehog (defExpectOptions)
import Protocols.Wishbone
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
  (KnownNat addressWidth) =>
  Gen a ->
  Gen (WishboneMasterRequest addressWidth a)
genWishboneTransfer genA =
  Gen.choice
    [ Read <$> genDefinedBitVector,
      Write <$> genDefinedBitVector <*> genA
    ]

--
-- 'id' circuit
--

idWriteWbSt ::
  forall a dom addressWidth.
  (BitPack a, NFDataX a) =>
  Circuit (Wishbone dom 'Standard addressWidth a) ()
idWriteWbSt = Circuit go
  where
    go (m2s, ()) = (reply <$> m2s, ())

    reply WishboneM2S {..}
      | busCycle && strobe && writeEnable =
        (emptyWishboneS2M @a)
          { acknowledge = True,
            readData = writeData
          }
      | busCycle && strobe = emptyWishboneS2M {acknowledge = True}
      | otherwise = emptyWishboneS2M

idWriteStModel ::
  (NFData a, Eq a, ShowX a) =>
  WishboneMasterRequest addressWidth a ->
  WishboneS2M a ->
  -- | pure state
  () ->
  Either String ()
idWriteStModel (Read _) s@WishboneS2M {..} ()
  | acknowledge && isLeft (hasX readData) = Right ()
  | otherwise =
    Left $ "Read should have been acknowledged with no DAT " <> showX s
idWriteStModel (Write _ a) s@WishboneS2M {..} ()
  | acknowledge && hasX readData == Right a = Right ()
  | otherwise =
    Left $ "Write should have been acknowledged with write-data as DAT " <> showX s

prop_idWriteSt_model :: Property
prop_idWriteSt_model =
  wishbonePropWithModel @System
    defExpectOptions
    idWriteStModel
    idWriteWbSt
    (genData $ genWishboneTransfer @10 genSmallInt)
    ()

--
-- memory element circuit
--

memoryWb ::
  forall ramSize dom a addressWidth.
  ( BitPack a,
    NFDataX a,
    1 <= ramSize,
    KnownDomain dom,
    KnownNat addressWidth,
    HiddenClockResetEnable dom,
    KnownNat ramSize,
    Default a
  ) =>
  Circuit (Wishbone dom 'Standard addressWidth a) ()
memoryWb = Circuit go
  where
    go (m2s, ()) = (reply m2s, ())

    reply ::
      Signal dom (WishboneM2S addressWidth (BitSize a `DivRU` 8) a) ->
      Signal dom (WishboneS2M a)
    reply request = do
      ack <- writeAck .||. readAck
      val <- readValue
      pure $ (emptyWishboneS2M @a) {acknowledge = ack, readData = val}
      where
        addr' = addr <$> request
        writeData' = writeData <$> request
        isWriteRequest = (\WishboneM2S {..} -> writeEnable && strobe && busCycle) <$> request
        write =
          mux
            isWriteRequest
            (Just <$> ((,) <$> addr' <*> writeData'))
            (pure Nothing)

        writeAck = isRising False isWriteRequest

        isReadRequest = (\WishboneM2S {..} -> writeEnable && strobe && busCycle) <$> request
        justReadRequest = isRising False isReadRequest

        readAck = register False justReadRequest

        readValue = blockRamU ClearOnReset (SNat @ramSize) (const def) addr' write

memoryWbModel ::
  (KnownNat addressWidth, Eq a, ShowX a, NFDataX a, NFData a, Default a) =>
  WishboneMasterRequest addressWidth a ->
  WishboneS2M a ->
  [(BitVector addressWidth, a)] ->
  Either String [(BitVector addressWidth, a)]
memoryWbModel (Read addr) s st@(lookup addr -> Just x)
  | readData s == x && acknowledge s = Right st
  | otherwise =
    Left $
      "Read from a known address did not yield the same value "
        <> showX x
        <> " : "
        <> showX s
memoryWbModel (Read addr) s st@(lookup addr -> Nothing)
  | acknowledge s && hasX (readData s) == Right def = Right st
  | otherwise =
    Left $ "Read from unknown address did no ACK with undefined result : " <> showX s
memoryWbModel (Write addr a) s st
  | acknowledge s = Right ((addr, a) : st)
  | otherwise = Left $ "Write should be acked : " <> showX s

prop_memoryWb_model :: Property
prop_memoryWb_model =
  withClockResetEnable clockGen resetGen enableGen $
    wishbonePropWithModel @System
      defExpectOptions
      memoryWbModel
      (memoryWb @256)
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
