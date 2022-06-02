{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Protocols.Wishbone.Hedgehog where

import           Clash.Prelude      as C hiding (cycle, not, (&&), (||))
import           Prelude            as P hiding (cycle)

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

data WishboneStandardState
  = WSSQuiet
  | WSSInCycleNoStrobe
  | WSSWaitForSlave
  -- TODO add a SlaveHolding state? Spec seems unclear

data WishboneStandardError
  = WSEMoreThanOneTerminationSignalAsserted
  | WSETerminationSignalOutsideOfCycle
  deriving (NFData, Generic, NFDataX, Show, ShowX, Eq)


nextStateStandard
  :: WishboneStandardState
  -> WishboneM2S addressWidth (BitSize a `DivRU` 8) a
  -> WishboneS2M a
  -> Either WishboneStandardError WishboneStandardState
nextStateStandard _ m2s s2m
  | P.length (filter ($ s2m) [acknowledge, err, retry]) > 1         = Left WSEMoreThanOneTerminationSignalAsserted
  | not (busCycle m2s) && (acknowledge s2m || err s2m || retry s2m) = Left WSETerminationSignalOutsideOfCycle
nextStateStandard WSSQuiet m2s _
  | busCycle m2s && P.not (strobe m2s)      = Right WSSInCycleNoStrobe
  | busCycle m2s && strobe m2s              = Right WSSWaitForSlave
  | otherwise                               = Right WSSQuiet
nextStateStandard WSSInCycleNoStrobe m2s _
  | not (busCycle m2s)         = Right WSSQuiet
  | busCycle m2s && strobe m2s = Right WSSWaitForSlave
  | otherwise                  = Right WSSInCycleNoStrobe
nextStateStandard WSSWaitForSlave m2s s2m
  | busCycle m2s && P.not (strobe m2s)      = Right WSSInCycleNoStrobe
  | not (busCycle m2s)                      = Right WSSQuiet
  | acknowledge s2m || err s2m || retry s2m = Right WSSQuiet
  | otherwise                               = Right WSSWaitForSlave


-- | Validate the input/output streams of a standard wishbone interface
--
-- Returns 'Right ()' when the interactions comply with the spec.
-- Returns 'Left (cycle, err)' when a spec violation was found.
validateStandard :: [WishboneM2S addressWidth (BitSize a `DivRU` 8) a] -> [WishboneS2M a] -> Either (Int, WishboneStandardError) ()
validateStandard m2s s2m = go 0 (P.zip m2s s2m) WSSQuiet
  where
    go _ [] _                 = Right ()
    go n ((fwd, bwd):rest) st = case nextStateStandard st fwd bwd of
      Left e    -> Left (n, e)
      Right st' -> go (n + 1) rest st'





data WishbonePipelineState = WishbonePipelineState
  { requests  :: Int
  , responses :: Int
  , cycle     :: Int
  , inCycle   :: Bool
  }

data WishbonePipelineError
  = WPECycleTerminatedWithPendingAcks
  | WPESlaveSignalsOutsideOfTransaction

nextStatePipelined
  :: WishbonePipelineState
  -> WishboneM2S addressWidth (BitSize a `DivRU` 8) a
  -> WishboneS2M a
  -> Either WishbonePipelineError WishbonePipelineState
nextStatePipelined st@WishbonePipelineState{..} m2s@WishboneM2S{..} s2m@WishboneS2M{..}
                                                      -- to decouple cycle tracking and tracking of STROBE
                                                      -- dependent signals, this case recurses to only do the
                                                      -- cycle tracking in this case.
  | not inCycle && busCycle                            = nextStatePipelined (st { inCycle = True, cycle = cycle + 1 }) m2s s2m
  | inCycle && not busCycle && (requests == responses) = Right $ st { inCycle = False, requests = 0, responses = 0 }
  | inCycle && not busCycle && (requests /= responses) = Left WPECycleTerminatedWithPendingAcks
  | not inCycle && not busCycle                        =
      if stall || acknowledge || err || retry then
        Left WPESlaveSignalsOutsideOfTransaction
      else
        Right st

  | strobe && stall                     = Right st
  | strobe && not stall && not slaveAck = Right $ st { requests = requests + 1 }
  | strobe && not stall && slaveAck     = Right $ st { requests = requests + 1, responses = responses + 1 }
  | not strobe && slaveAck              = Right $ st { responses = responses + 1 }
  | not strobe && not slaveAck          = Right st
  | otherwise                           = Right st

  where
    slaveAck = acknowledge || err || retry


-- | Validate the input/output streams of a standard wishbone interface
--
-- Returns 'Right ()' when the interactions comply with the spec.
-- Returns 'Left (cycle, err)' when a spec violation was found.
validatePipelined :: [WishboneM2S addressWidth (BitSize a `DivRU` 8) a] -> [WishboneS2M a] -> Either (Int, WishbonePipelineError) ()
validatePipelined m2s s2m = go 0 (P.zip m2s s2m) $ WishbonePipelineState { cycle = 0, inCycle = False, requests = 0, responses = 0 }
  where
    go _ [] _                 = Right ()
    go n ((fwd, bwd):rest) st = case nextStatePipelined st fwd bwd of
      Left e    -> Left (n, e)
      Right st' -> go (n + 1) rest st'









validateStallingStandardCircuit
  :: forall dom a addressWidth
  . (C.ShowX a, Show a, C.KnownNat addressWidth, C.KnownDomain dom, C.KnownNat (C.BitSize a))
  => ExpectOptions
  -> H.Gen [WishboneM2S addressWidth (BitSize a `DivRU` 8) a]
  -> Circuit (Wishbone dom 'Standard addressWidth a) ()
  -> H.Property
validateStallingStandardCircuit eOpts gen circ = H.property $ do
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


validateStallingPipelineCircuit
  :: forall dom a addressWidth
  . (C.ShowX a, Show a, C.KnownNat addressWidth, C.KnownDomain dom, C.KnownNat (C.BitSize a))
  => ExpectOptions
  -> H.Gen [WishboneM2S addressWidth (BitSize a `DivRU` 8) a]
  -> Circuit (Wishbone dom 'Pipelined addressWidth a) ()
  -> H.Property
validateStallingPipelineCircuit eOpts gen circ = H.property $ do
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
    lhsStallC = stallC @(Wishbone dom 'Pipelined addressWidth a) simConfig lhsStalls
    stalledProtocol =
        lhsStallC |> circ
    sampled = thing dat stalledProtocol

  validateStandard dat sampled === Right ()

  where

    thing :: [WishboneM2S addressWidth (BitSize a `DivRU` 8) a] -> Circuit (Wishbone dom 'Pipelined addressWidth a) () -> [WishboneS2M a]
    thing input (Circuit f) =
      let in' = fromList_lazy input
          (out, _) = f (in', ())
      in sample_lazy out
