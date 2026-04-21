-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}

-- | Extra utilities for 'Df'.
module Protocols.Df.Extra where

import Clash.Prelude
import Data.Bifunctor (Bifunctor (bimap))
import Data.Maybe (isJust, isNothing)
import Protocols

-- | Indicates whether the downstream is ready to accept data
type Ready = Bool

-- | State of 'skid'.
data SkidState a
  = InReset
  | Empty
  | Full a
  deriving (Generic, NFDataX)

{- | A skid buffer that can store a single element and will acknowledge data if it can
store it. Also indicates when it is ready to accept data. In practice, the ready signal is
the exact same as the ack signal, but 'Df' disallows interpreting the ack signal as a
this-component-is-ready-to-accept-data signal.

This skid buffer is implemented in terms of 'moore', so it cuts up combinational paths.
-}
skid ::
  forall dom a.
  (NFDataX a, HiddenClockResetEnable dom) =>
  Circuit (Df dom a) (Df dom a, CSignal dom Ready)
skid = Circuit go
 where
  go (dataIn, (ackIn, _)) = (ackOut, (dataOut, readyOut))
   where
    (ackOut, dataOut, readyOut) = mooreB goState goOutput InReset (dataIn, ackIn)

  goState :: SkidState a -> (Maybe a, Ack) -> SkidState a
  goState InReset _ = Empty
  goState Empty (Nothing, _) = Empty
  goState Empty (Just dat, _) = Full dat
  goState s@(Full _) (_, ~(Ack ack)) = if ack then Empty else s

  goOutput :: SkidState a -> (Ack, Maybe a, Bool)
  goOutput s = let ack = isEmpty s in (Ack ack, toMaybe s, ack)

  isEmpty :: SkidState a -> Bool
  isEmpty Empty = True
  isEmpty _ = False

  toMaybe :: SkidState a -> Maybe a
  toMaybe (Full a) = Just a
  toMaybe _ = Nothing

-- | State of 'bypassFifo'.
data BypassState a maxDelay
  = BypassState
  { inReset :: Bool
  , stored :: Maybe a
  , count :: Index (maxDelay + 1)
  }
  deriving (Generic, NFDataX)

{- | Fifos inherently have latency, this circuit allows you to bypass the fifo when it is empty
to allow for 0 latency communication when possible, while still adhering to the Df protocol.
-}
bypassFifo ::
  forall dom maxDelay a.
  (HiddenClockResetEnable dom, NFDataX a, 1 <= maxDelay) =>
  SNat maxDelay ->
  Circuit (Df dom a) (Df dom a) ->
  Circuit (Df dom a) (Df dom a)
bypassFifo SNat fifoCircuit = circuit $ \inp -> do
  fifoOut <- fifoCircuit -< fifoIn
  (out, fifoIn) <- bypassCkt -< (inp, fifoOut)
  idC -< out
 where
  bypassCkt =
    Circuit
      ( bimap unbundle unbundle
          . unbundle
          . mealy go initState
          . bundle
          . bimap bundle bundle
      )
  initState :: BypassState a maxDelay
  initState = BypassState{inReset = True, stored = Nothing, count = 0}

  -- Reset state
  go state ~(~(inp, fifoOut), ~(Ack outAck, Ack fifoInAck))
    | state.inReset =
        (initState{inReset = False}, ((Ack False, Ack False), (Nothing, Nothing)))
    | isNothing state.stored && state.count == 0 =
        let
          inpAck = Ack True
          fifoOutAck = Ack False
          out = inp
          fifoIn = Nothing
          -- If we receive backpressure while trying to bypass, buffer the input
          -- to adhere to the Df protocol.
          nextReg
            | isJust inp && not outAck = inp
            | otherwise = Nothing

          nextCount = 0
          nextState = BypassState{inReset = False, stored = nextReg, count = nextCount}
         in
          (nextState, ((inpAck, fifoOutAck), (out, fifoIn)))
    | isJust state.stored =
        let
          fifoIn = inp
          inpAck = Ack fifoInAck
          out = state.stored
          fifoOutAck = Ack False

          -- We receive backpressure on a buffered valued, if we receive a new value we have to
          -- put it in the fifo and thus increase the count
          nextCount
            | isJust inp = maxBound
            | otherwise = state.count

          nextReg = if outAck then Nothing else state.stored
          nextState = BypassState{inReset = False, stored = nextReg, count = nextCount}
         in
          (nextState, ((inpAck, fifoOutAck), (out, fifoIn)))
    -- Fifo is not empty, count is nonzero.
    | otherwise =
        let
          inpAck = Ack fifoInAck
          fifoOutAck = Ack outAck
          out = fifoOut
          fifoIn = inp

          -- When the fifo produces a sample, reset the count to maxBound
          nextCount
            | isJust fifoOut = maxBound
            | otherwise = satPred SatZero state.count

          nextReg = Nothing
          nextState = BypassState{inReset = False, stored = nextReg, count = nextCount}
         in
          (nextState, ((inpAck, fifoOutAck), (out, fifoIn)))

{- | Will stall the next incoming transaction until the 'Bool' is 'True'. If it becomes 'False'
 while a transaction is being processed it will not be affected, but the next transaction will
be blocked until it is 'True' again.
-}
stallNext ::
  forall dom a.
  (HiddenClockResetEnable dom, NFDataX a) =>
  -- | Blocks when False
  Signal dom Bool ->
  Circuit (Df dom a) (Df dom a)
stallNext rdyS = circuit $ \req -> do
  ckt -< (req, Fwd rdyS)
 where
  ckt :: Circuit (Df dom a, CSignal dom Bool) (Df dom a)
  ckt = Circuit goS

  goS ((datS, rdyS'), ack) = ((ackOutS, ()), datOutS)
   where
    (ackOutS, datOutS) = unbundle $ mealy go False $ bundle $ (bundle (datS, rdyS'), ack)

  go offering ((dat, rdy), Ack ackIn) = (nextOffering, (ackOut, datOut))
   where
    passThrough = offering || rdy
    datOut
      | passThrough = dat
      | otherwise = Nothing

    nextOffering
      | isJust dat && passThrough = not ackIn
      | otherwise = False

    ackOut = Ack (passThrough && ackIn)
