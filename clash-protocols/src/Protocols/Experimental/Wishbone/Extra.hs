-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0

-- | Extra utilities for 'Wishbone'.
module Protocols.Experimental.Wishbone.Extra (delayWishbone) where

import Prelude hiding ((&&))

import Clash.Prelude
import Protocols
import Protocols.Experimental.Wishbone

-- | State for 'delayWishbone'.
data DelayWishboneState aw n
  = WaitingForManager
  | WaitingForSubordinate (WishboneM2S aw n)
  | AcknowledgingManager (WishboneS2M n)
  deriving (Generic, NFDataX)

{- | Breaks the combinatorial path between a Wishbone manager and subordinate by inserting
a Moore machine. It introduces two cycles of delay for each transaction, one to forward
the request from manager to subordinate, and one to forward the response from subordinate
to manager.
-}
delayWishbone ::
  forall dom aw n.
  (HiddenClockResetEnable dom, KnownNat aw, KnownNat n) =>
  Circuit (Wishbone dom 'Standard aw n) (Wishbone dom 'Standard aw n)
delayWishbone = Circuit go
 where
  go ::
    ( Signal dom (WishboneM2S aw n)
    , Signal dom (WishboneS2M n)
    ) ->
    ( Signal dom (WishboneS2M n)
    , Signal dom (WishboneM2S aw n)
    )
  go = mooreB mooreTransfer mooreOut WaitingForManager
   where
    mooreTransfer ::
      DelayWishboneState aw n ->
      (WishboneM2S aw n, WishboneS2M n) ->
      DelayWishboneState aw n
    mooreTransfer state inps@(m2s0, s2m0) = m2s0 `seqX` s2m0 `seqX` transfer state inps

    transfer ::
      DelayWishboneState aw n ->
      (WishboneM2S aw n, WishboneS2M n) ->
      DelayWishboneState aw n
    transfer WaitingForManager (m2s, _s2m)
      | m2s.busCycle && m2s.strobe = WaitingForSubordinate m2s
    transfer (WaitingForSubordinate _) (_m2s, s2m)
      | hasTerminateFlag s2m = AcknowledgingManager s2m
    transfer (AcknowledgingManager _) _ = WaitingForManager
    transfer s _ = s

    mooreOut ::
      DelayWishboneState aw n ->
      (WishboneS2M n, WishboneM2S aw n)
    mooreOut WaitingForManager = (emptyWishboneS2M, emptyWishboneM2S)
    mooreOut (WaitingForSubordinate m2s) = (emptyWishboneS2M, m2s)
    mooreOut (AcknowledgingManager s2m) = (s2m, emptyWishboneM2S)
