{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}
module Protocols.Wishbone.Hedgehog where

import           Clash.Prelude          as C hiding (cycle, not, (&&), (||))
import           Clash.Signal.Internal  (Signal ((:-)))
import           Prelude                as P hiding (cycle)

-- hedgehog
import           Control.DeepSeq        (NFData)
import qualified Hedgehog               as H
import qualified Hedgehog.Gen           as Gen
import qualified Hedgehog.Range         as Range


-- me
import           Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Bifunctor         as B
import           Data.Maybe             (isJust)
import           Debug.Trace
import           Hedgehog               ((===))
import           Protocols
import           Protocols.Hedgehog
import           Protocols.Wishbone
import           Text.Printf            (printf)

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
  deriving (NFData, Generic, NFDataX, Show, ShowX, Eq)

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

validateStallingPipelineCircuitInner
  :: forall dom a addressWidth
  . (C.ShowX a, Show a, C.NFDataX a, C.KnownNat addressWidth, C.HiddenClockResetEnable dom, C.KnownNat (C.BitSize a))
  => ExpectOptions
  -> [WishbonePipelinedCycleRequests addressWidth a]
  -> [PipelineSlaveStallMode]
  -> Circuit (Wishbone dom 'Pipelined addressWidth a) ()
  -> Either (Int, WishbonePipelineError) ()
validateStallingPipelineCircuitInner eOpts reqs stalls slave =
  let
    simConfig = def {resetCycles = eoResetCycles eOpts}
    stalledC = stallPipelinedC simConfig stalls
    stallingSlave = stalledC |>
       slave
    master = driveWishbonePipelinedMaster simConfig reqs

    (m2s, s2m) = observeComposedWishboneCircuit (eoTimeout eOpts) master stallingSlave
  in

  validatePipelined m2s s2m


validateStallingPipelineCircuit
  :: forall dom a addressWidth
  . (C.ShowX a, Show a, C.NFDataX a, C.KnownNat addressWidth, C.HiddenClockResetEnable dom, C.KnownNat (C.BitSize a))
  => ExpectOptions
  -> H.Gen [WishbonePipelinedCycleRequests addressWidth a]
  -> Circuit (Wishbone dom 'Pipelined addressWidth a) ()
  -> H.Property
validateStallingPipelineCircuit eOpts gen slave = H.property $ do
  dat <- H.forAll gen
  let len = P.length dat

  stalls <- H.forAll $ Gen.list (Range.singleton len) genPipelineSlaveStallMode

  validateStallingPipelineCircuitInner eOpts dat stalls slave === Right ()



data PipelineSlaveStallMode
    = AckAndStall Int
    -- ^ Immediately ACK a request but stall the master for N cycles afterwards.
    | StallThenAck Int Int
    -- ^ Stall the master and wait before ACK-ing a request.
    --   - first field is the number of cycles to assert STALL
    --   - second field is the number of cycles to STALL before sending ACK
    deriving (C.Generic, C.NFDataX, Show, C.ShowX)

genPipelineSlaveStallMode :: H.Gen PipelineSlaveStallMode
genPipelineSlaveStallMode = do
  let genSmallInt = Gen.integral (Range.linear 0 10)
  Gen.choice [ AckAndStall <$> genSmallInt, StallThenAck <$> genSmallInt <*> genSmallInt ]

data PipelinedStallState a = PipelinedStallState
  { response   :: Maybe (WishboneS2M a)
  , stalls     :: [PipelineSlaveStallMode]
  , resetN     :: Int
  , acked      :: Bool
  , allowedReq :: Bool
  }
  deriving (C.Generic, C.NFDataX, C.ShowX)

stallPipelinedC
  :: forall dom addressWidth a. (C.HiddenClockResetEnable dom, C.KnownNat addressWidth, C.NFDataX a, KnownNat (BitSize a), C.ShowX a)
  => SimulationConfig
  -> [PipelineSlaveStallMode]
  -> Circuit (Wishbone dom 'Pipelined addressWidth a) (Wishbone dom 'Pipelined addressWidth a)
stallPipelinedC config stalls = Circuit $ B.bimap (wishboneS2M :-) (wishboneM2S :-) . C.mealyB run' initState
  where
    initState :: PipelinedStallState a
    initState = PipelinedStallState Nothing stalls (resetCycles config) False False

    run' st (m2s, s2m) = let (st', s2m', m2s') = run st m2s s2m in (st', (s2m', m2s'))

    run st@PipelinedStallState{..} m2s s2m
      | resetN > 0  = (st { resetN = resetN - 1 }, s2m, m2s)

    -- no more stalls to perform, just pass through the signals
    run st@PipelinedStallState{stalls = []} m2s s2m = (st, s2m, m2s)

    -- not in a cycle, just pass through
    run st m2s s2m
      | not (busCycle m2s) = (st, s2m, m2s)

    --
    -- AckAndStall
    --

    run st@PipelinedStallState{ stalls = (AckAndStall _):_, acked = False, .. } m2s s2m
      -- request comes in, but no slave response yet -> stall
      | strobe m2s && not (terminationSignal s2m) = trace "3" (st, s2m { stall = True }, m2s')
      | strobe m2s && terminationSignal s2m       = trace "4" (st { response = Just s2m, acked = True }, s2m { stall = True }, m2s')
      | not (strobe m2s) && not (terminationSignal s2m) = trace "5" (st, wishboneS2M { stall = True }, m2s')
      | not (strobe m2s) && terminationSignal s2m       = trace "6" (st { response = Just s2m, acked = True }, s2m, m2s')
      where
        m2s' = if isJust response || terminationSignal s2m then wishboneM2S { busCycle = True } else m2s

    run st@PipelinedStallState{ stalls = (AckAndStall n):stalls, acked = True } m2s s2m
      | n == 0 && not (strobe m2s) = trace "7" (st { stalls = stalls, acked = False, response = Nothing }, wishboneS2M, m2s)
      | n == 0 && strobe m2s       = trace "8" (st { stalls = stalls, acked = False, response = Nothing }, wishboneS2M { stall = True }, m2s)
      | n /= 0 && terminationSignal s2m = C.error "unexpected slave reply"
      -- stall no matter whether STB is asserted or not
      | n /= 0                     = trace "9" (st { stalls = AckAndStall (n - 1):stalls }, wishboneS2M { stall = True }, m2s { strobe = False })

{-

    --
    -- StallThenAck
    --

    -- request, no ack from slave yet, if supposed to be stalling, do that first.
    -- continue to stall until ack is available
    run st@PipelinedStallState{ stalls = (StallThenAck n i):stalls, response = Nothing } m2s s2m
      -- work on remaining stalls first
      | strobe m2s && n >  0 = trace "8" (st { stalls = StallThenAck (n - 1) i:stalls }, s2m { stall = True })
      -- done with stalls? still no reply? keep stalling
      | strobe m2s && n == 0 = trace "9" (st, s2m { stall = True })

    -- request, already got ack from slave, if supposed to be stalling, do that first.
    -- if not supposed to stall anymore
    run st@PipelinedStallState{ stalls = (StallThenAck n i):stalls, response = Just resp, acked = False, .. } m2s s2m
      -- work on remaining stalls first
      | strobe m2s && n > 0  = trace "10" (st { stalls = StallThenAck (n - 1) i:stalls }, s2m { stall = True })
      -- done with stalls? send reply
      | strobe m2s && n == 0 && not allowedReq = trace "11" (st { allowedReq = True }, s2m { err = False, retry = False, acknowledge = False })
      | strobe m2s && n == 0 && allowedReq     = trace "12" (st { acked = True }, resp { stall = True })

    -- stalled already, ack send
    -- if not supposed to stall anymore
    run st@PipelinedStallState{ stalls = (StallThenAck 0 i):stalls, acked = False } m2s s2m
      -- work on remaining stalls first
      | strobe m2s && i > 0  = trace "13" (st { stalls = StallThenAck 0 (i - 1):stalls }, s2m { stall = True })
      -- last stall, continue
      | strobe m2s && i == 0 = trace "14" (st { acked = False, allowedReq = False, stalls = stalls }, s2m { stall = True })


    -- run _ _ = C.errorX "Should not happen"

    -}
    run st m2s s2m = -- trace (printf "other:\n\t%s\n\t%s\n\t%s" (showX m2s) (showX s2m) (showX st))
      trace "other"
      (st, s2m, m2s)







observeComposedWishboneCircuit
  :: (KnownDomain dom)
  => Maybe Int
  -> Circuit () (Wishbone dom mode addressWidth a)
  -> Circuit (Wishbone dom mode addressWidth a) ()
  -> (
      [WishboneM2S addressWidth (BitSize a `DivRU` 8) a],
      [WishboneS2M a]
    )
observeComposedWishboneCircuit Nothing (Circuit master) (Circuit slave) =
  let ~((), m2s) = master ((), s2m)
      ~(s2m, ()) = slave (m2s, ())
  in (sample_lazy m2s, sample_lazy s2m)
observeComposedWishboneCircuit (Just n) (Circuit master) (Circuit slave) =
  let ~((), m2s) = master ((), s2m)
      ~(s2m, ()) = slave (m2s, ())
  in (sampleN_lazy n m2s, sampleN_lazy n s2m)


observeComposedWishboneCircuitVCD
  :: (KnownDomain dom, NFDataX a, KnownNat (BitSize a), KnownNat addressWidth)
  => (Int, Int)
  -> Circuit () (Wishbone dom mode addressWidth a)
  -> Circuit (Wishbone dom mode addressWidth a) ()
  -> IO String
observeComposedWishboneCircuitVCD (resets, count) (Circuit master) (Circuit slave) = do
  let ~((), m2s) = master ((), s2m)
      ~(s2m, ()) = slave (m2s, ())
      signal = bundle (m2s, s2m)
  res <- dumpVCD (resets, count) signal ["CYC", "STB", "ACK", "STALL", "WE"]
  case res of
    Left s  -> pure s
    Right _ -> pure "Failed to create trace"



type WishbonePipelinedCycleRequests addressWidth dat = [WishboneMasterRequest addressWidth dat]

data WishboneMasterRequest addressWidth dat
  = Read (BitVector addressWidth)
  | Write (BitVector addressWidth) dat
  deriving (Show)

data WishbonePipelinedDriverState addressWidth dat = WishbonePipelinedDriverState
  { reqsSent     :: Int
  , acksReceived :: Int
  , lastReq      :: Maybe (WishboneMasterRequest addressWidth dat)
  }

driveWishbonePipelinedMaster
  :: forall dom addressWidth dat. (KnownDomain dom, KnownNat addressWidth, KnownNat (BitSize dat), NFDataX dat)
  => SimulationConfig
  -> [WishbonePipelinedCycleRequests addressWidth dat]
  -> Circuit () (Wishbone dom 'Pipelined addressWidth dat)
driveWishbonePipelinedMaster SimulationConfig{resetCycles} cycles = Circuit $
    ((),)
    . (wishboneM2S :-)
    . go resetCycles (WishbonePipelinedDriverState {reqsSent=0, acksReceived=0, lastReq = Nothing}) cycles
    . sample_lazy
    . snd
  where

    reqToSignals
      :: forall addressWidth dat. (KnownNat addressWidth, KnownNat (BitSize dat), NFDataX dat)
      => WishboneMasterRequest addressWidth dat
      -> WishboneM2S addressWidth (BitSize dat `DivRU` 8) dat
    reqToSignals (Read addr) = (wishboneM2S @addressWidth)
      { busCycle = True, strobe = True, addr = addr, writeEnable = False }
    reqToSignals (Write addr content) = (wishboneM2S @addressWidth @dat)
      { busCycle = True, strobe = True, addr = addr, writeData = content, writeEnable = True }

    go :: Int
       -> WishbonePipelinedDriverState addressWidth dat
       -> [WishbonePipelinedCycleRequests addressWidth dat]
       -> [WishboneS2M dat]
       -> Signal dom (WishboneM2S addressWidth (BitSize dat `DivRU` 8) dat)
    -- perform resets
    go resets st cycles ~(rep:replies)
      | resets > 0  = wishboneM2S :- (rep `C.seqX` go (resets - 1) st cycles replies)
    -- no more cycles to work on
    go _ st [] ~(rep:replies) = wishboneM2S :- (rep `C.seqX` go 0 st [] replies)

    go _ st@WishbonePipelinedDriverState{..} ([]:cycles) ~(rep:replies)
      -- finished the cycle
      | reqsSent == acksReceived && not (stall rep) = wishboneM2S :- (rep `C.seqX` go 0 (st { reqsSent = 0, acksReceived = 0, lastReq = Nothing}) cycles replies)
      -- finished but slave is still stalling
      | reqsSent == acksReceived && stall rep       = wishboneM2S { busCycle = True } :- (rep `C.seqX` go 0 (st { lastReq = Nothing }) ([]:cycles) replies)


    go _ st@WishbonePipelinedDriverState{lastReq = Nothing, ..} ([]:cycles) ~(rep:replies)
      -- still waiting for acks but no last request? Shouldn't be possible.
      | reqsSent /= acksReceived = wishboneM2S { busCycle = True } :- (rep `C.seqX` go 0 st ([]:cycles) replies)


    go _ st@WishbonePipelinedDriverState{lastReq = Just req, ..} ([]:cycles) ~(rep:replies)
      -- got an ack, not stalling
      | reqsSent /= acksReceived && acknowledge rep && not (stall rep) =
          if acksReceived + 1 == reqsSent then
            -- end cycle
            wishboneM2S :- (rep `C.seqX` go 0 (st { reqsSent = 0, acksReceived = 0, lastReq = Nothing}) cycles replies)
          else
            -- still more acks to wait for
            reqToSignals req :- (rep `C.seqX` go 0 (st { acksReceived = acksReceived + 1 }) ([]:cycles) replies)

      -- got an ack while stalling
      | reqsSent /= acksReceived && acknowledge rep && stall rep =
          if acksReceived + 1 == reqsSent then
            -- slave acknowledged last request but is still stalling, insert wait state
            wishboneM2S { busCycle = True } :- (rep `C.seqX` go 0 (st { acksReceived = acksReceived + 1 }) ([]:cycles) replies)
          else
            -- still pending acks, keep sending last request
            reqToSignals req :- (rep `C.seqX` go 0 (st { acksReceived = acksReceived + 1 }) ([]:cycles) replies)

      -- still waiting for acks, stalling, so keep sending last req.
      | reqsSent /= acksReceived && stall rep       = reqToSignals req :- (rep `C.seqX` go 0 (st { reqsSent = 0, acksReceived = 0}) ([]:cycles) replies)
      -- not stalling, to prevent accidentally sending the request again insert a wait state.
      | reqsSent /= acksReceived && not (stall rep) = wishboneM2S { busCycle = True } :- (rep `C.seqX` go 0 (st { reqsSent = 0, acksReceived = 0}) ([]:cycles) replies)

    -- lastreq not populated, so this is the first request in the cycle
    go _ st@WishbonePipelinedDriverState{lastReq = Nothing} ((req:reqs):cycles) ~(rep:replies)
      | {- stall rep || -} terminationSignal rep =
          wishboneM2S { busCycle = True } :- (rep `C.seqX` go 0 st ((req:reqs):cycles) replies)
          -- C.errorX "STALL or terminating signal outside of cycle"
      -- send out new request
      | otherwise = reqToSignals req :- (rep `C.seqX` go 0 (st { reqsSent = 1, acksReceived = 0, lastReq = Just req }) (reqs:cycles) replies)


    -- cycle was already started, 'last' is the request that needs to be ACKed/accepted eventually and 'req' is the next one.
    go _ st@WishbonePipelinedDriverState{lastReq = Just last, ..} ((req:reqs):cycles) ~(rep:replies)
      | stall rep = reqToSignals last :- (rep `C.seqX` go 0 st ((req:reqs):cycles) replies)
      -- not stalled but also no termination signal => "accepted" the request
      | not (stall rep) && not (terminationSignal rep) = reqToSignals last :- (rep `C.seqX` go 0 (st { reqsSent = reqsSent + 1 }) ((req:reqs):cycles) replies)
      -- simple stall, no termination, keep sending same request
      | stall rep && not (terminationSignal rep) = reqToSignals last :- (rep `C.seqX` go 0 st ((req:reqs):cycles) replies)
      -- stalling but sent ACK/ERR, keep sending last request
      | stall rep && (acknowledge rep || err rep) = reqToSignals last :- (rep `C.seqX` go 0 (st { acksReceived = acksReceived + 1 }) ((req:reqs):cycles) replies)

      | stall rep && retry rep = C.error "HELP I don't know what to do with a RETY in pipelined mode AAAAA"

      -- not stalling, so accepting the next request, termination, send next request
      | not (stall rep) && (acknowledge rep || err rep) = reqToSignals req :- (rep `C.seqX` go 0 (st { acksReceived = acksReceived + 1, reqsSent = reqsSent + 1 }) (reqs:cycles) replies)

      | not (stall rep) && retry rep = C.error "HELP I don't know what to do with a RETY in pipelined mode AAAAA"


    go _ _ _ _ = error "not implemented"
