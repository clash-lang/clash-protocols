{-# LANGUAGE FlexibleContexts #-}
module Protocols.Wishbone.Hedgehog where

import           Clash.Prelude      as C hiding ((&&), (||))
import           Prelude            as P

-- hedgehog
import           Control.DeepSeq    (NFData)
import qualified Hedgehog           as H
import qualified Hedgehog.Gen       as Gen
import qualified Hedgehog.Range     as Range

-- me
import           Hedgehog           ((===))
import           Protocols
import           Protocols.Hedgehog
import           Protocols.Wishbone

data WishboneState
  = Quiet
  | InCycleNoStrobe
  | WaitForSlave
  -- TODO add a SlaveHolding state? Spec seems unclear

data WishboneError
  = MoreThanOneTerminationSignalAsserted
  | TerminationSignalOutsideOfCycle
  deriving (NFData, Generic, NFDataX, Show, ShowX, Eq)


nextStateStandard
  :: WishboneState
  -> WishboneM2S addressWidth (BitSize a `DivRU` 8) a
  -> WishboneS2M a
  -> Either WishboneError WishboneState
nextStateStandard _ m2s s2m
  | P.length (filter ($ s2m) [acknowledge, err, retry]) > 1           = Left MoreThanOneTerminationSignalAsserted
  | P.not (busCycle m2s) && (acknowledge s2m || err s2m || retry s2m) = Left TerminationSignalOutsideOfCycle
nextStateStandard Quiet m2s _
  | busCycle m2s && P.not (strobe m2s)      = Right InCycleNoStrobe
  | busCycle m2s && strobe m2s              = Right WaitForSlave
  | otherwise                               = Right Quiet
nextStateStandard InCycleNoStrobe m2s _
  | P.not (busCycle m2s)         = Right Quiet
  | busCycle m2s && strobe m2s = Right WaitForSlave
  | otherwise                  = Right InCycleNoStrobe
nextStateStandard WaitForSlave m2s s2m
  | busCycle m2s && P.not (strobe m2s)      = Right InCycleNoStrobe
  | P.not (busCycle m2s)                    = Right Quiet
  | acknowledge s2m || err s2m || retry s2m = Right Quiet
  | otherwise                               = Right WaitForSlave


-- | Validate the input/output streams of a standard wishbone interface
--
-- Returns 'Right ()' when the interactions comply with the spec.
-- Returns 'Left (cycle, err)' when a spec violation was found.
validateStandard :: [WishboneM2S addressWidth (BitSize a `DivRU` 8) a] -> [WishboneS2M a] -> Either (Int, WishboneError) ()
validateStandard m2s s2m = go 0 (P.zip m2s s2m) Quiet
  where
    go _ [] _                 = Right ()
    go n ((fwd, bwd):rest) st = case nextStateStandard st fwd bwd of
      Left err  -> Left (n, err)
      Right st' -> go (n + 1) rest st'




validateStallingCircuit
  :: forall dom a addressWidth
  . (C.ShowX a, Show a, C.KnownNat addressWidth, C.KnownDomain dom, C.KnownNat (C.BitSize a))
  => ExpectOptions
  -> H.Gen [WishboneM2S addressWidth (BitSize a `DivRU` 8) a]
  -> Circuit (Wishbone dom 'Standard addressWidth a) ()
  -> H.Property
validateStallingCircuit eOpts gen circ = H.property $ do
  dat <- H.forAll gen
  let n = P.length dat

  -- TODO: Different distributions?
  let genStall = Gen.integral (Range.linear 0 10)

  -- Generate stalls for LHS part of the protocol. The first line determines
  -- whether to stall or not. The second determines how many cycles to stall
  -- on each _valid_ cycle.
  lhsStallModes <- H.forAll (sequenceA (genStallMode :> Nil))
  lhsStalls <- H.forAll (traverse (genStalls genStall n) lhsStallModes)


  let
    simConfig = def {resetCycles = eoResetCycles eOpts}
    lhsStallC = stallC @(Wishbone dom 'Standard addressWidth a) simConfig lhsStalls
    stalledProtocol =
        lhsStallC |> circ
    sampled = thing dat stalledProtocol

  validateStandard dat sampled === Right ()

  where

    thing :: [WishboneM2S addressWidth (BitSize a `DivRU` 8) a] -> Circuit (Wishbone dom 'Standard addressWidth a) () -> [WishboneS2M a]
    thing input (Circuit f) =
      let in' = fromList_lazy input
          (out, _) = f (in', ())
      in sample_lazy out


