-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}

module Tests.Protocols.Wishbone.Extra where

import Clash.Prelude

import Data.Map.Strict (Map)
import Hedgehog (Gen, Property)
import Protocols
import Protocols.Hedgehog (defExpectOptions)
import Protocols.Wishbone
import Protocols.Wishbone.Extra (delayWishbone)
import Protocols.Wishbone.Standard.Hedgehog (
  WishboneMasterRequest (Read, Write),
  wishbonePropWithModel,
 )
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import Data.Map.Strict qualified as Map
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

type AddressWidth = 4

-- | Merge bytes from @new@ into @old@ using a byte-enable mask.
mergeWithMask :: BitVector 32 -> BitVector 32 -> BitVector 4 -> BitVector 32
mergeWithMask (unpack -> old) (unpack -> new) (unpack -> mask) =
  pack (mux @(Vec 4) @(BitVector 8) mask new old)

{- | A simple synchronous register-file Wishbone subordinate for testing.
Immediately acknowledges every valid transaction and initialises all
addresses to zero.
-}
simpleWbMemory ::
  (HiddenClockResetEnable dom) =>
  Circuit (Wishbone dom 'Standard AddressWidth 4) ()
simpleWbMemory = Circuit go
 where
  go (m2sS, ()) = (mealy step initMem m2sS, ())
  initMem = repeat 0 :: Vec 16 (BitVector 32)

  step ::
    Vec 16 (BitVector 32) ->
    WishboneM2S AddressWidth 4 ->
    (Vec 16 (BitVector 32), WishboneS2M 4)
  step mem m2s
    | m2s.busCycle && m2s.strobe =
        let
          idx = unpack m2s.addr :: Index 16
          old = mem !! idx
          mem' =
            if m2s.writeEnable
              then replace idx (mergeWithMask old m2s.writeData m2s.busSelect) mem
              else mem
         in
          (mem', (emptyWishboneS2M @4){readData = old, acknowledge = True})
    | otherwise = (mem, (emptyWishboneS2M @4))

{- | Test that @delayWishbone@ correctly forwards Wishbone transactions
without data corruption. Connects @delayWishbone@ in front of a simple
register-file and verifies that reads and writes produce the expected
responses.
-}
prop_delayWishbone :: Property
prop_delayWishbone =
  H.property
    $ withClockResetEnable @System clockGen resetGen enableGen
    $ wishbonePropWithModel
      defExpectOptions
      model
      (delayWishbone |> simpleWbMemory)
      genInputs
      Map.empty
 where
  model ::
    WishboneMasterRequest AddressWidth 4 ->
    WishboneS2M 4 ->
    Map (BitVector AddressWidth) (BitVector 32) ->
    Either String (Map (BitVector AddressWidth) (BitVector 32))
  model _ resp _ | resp.err = Left "Unexpected bus error"
  model (Read addr _) resp mem
    | resp.acknowledge =
        let expected = Map.findWithDefault 0 addr mem
         in if resp.readData == expected
              then Right mem
              else Left "Data mismatch on read"
  model (Write addr sel dat) resp mem
    | resp.acknowledge =
        Right (Map.insert addr (mergeWithMask (Map.findWithDefault 0 addr mem) dat sel) mem)
  model _ _ mem = Right mem

  genInputs :: Gen [WishboneMasterRequest AddressWidth 4]
  genInputs =
    Gen.list (Range.linear 0 32)
      $ Gen.choice
        [ Read <$> genBoundedIntegral <*> genBoundedIntegral
        , Write <$> genBoundedIntegral <*> genBoundedIntegral <*> genBoundedIntegral
        ]

  genBoundedIntegral :: forall a. (Integral a, Bounded a) => Gen a
  genBoundedIntegral = Gen.integral Range.constantBounded

tests :: TestTree
tests = $(testGroupGenerator)
