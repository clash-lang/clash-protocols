{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
Copyright  :  (C) 2024, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Provides a packet arbiter and dispatcher, for merging and splitting packet streams.
-}
module Protocols.PacketStream.Routing (
  packetArbiterC,
  packetDispatcherC,
  routeBy,
) where

import Clash.Prelude

import Protocols
import qualified Protocols.Df as Df
import Protocols.PacketStream.Base

import qualified Data.Bifunctor as B
import Data.Maybe

-- | Merges multiple packet streams into one, respecting packet boundaries.
packetArbiterC ::
  forall dataWidth sources meta dom.
  (HiddenClockResetEnable dom) =>
  (KnownNat sources) =>
  (1 <= sources) =>
  -- | Determines the mode of arbitration. See `Df.CollectMode`
  Df.CollectMode ->
  Circuit
    (Vec sources (PacketStream dom dataWidth meta))
    (PacketStream dom dataWidth meta)
packetArbiterC mode =
  Circuit (B.first unbundle . mealyB go (maxBound, True) . B.first bundle)
 where
  go (i, first) (fwds, bwd@(PacketStreamS2M ack)) = ((i', continue), (bwds, fwd))
   where
    bwds = replace i bwd (repeat (PacketStreamS2M False))
    fwd = fwds !! i

    -- We may only switch sources if we are not currently in the middle
    -- of forwarding a packet.
    continue = case (fwd, mode) of
      (Nothing, Df.NoSkip) -> False
      (Nothing, _) -> first
      (Just transferIn, _) -> isJust (_last transferIn) && ack

    i' = case (mode, continue) of
      (_, False) -> i
      (Df.NoSkip, _) -> satSucc SatWrap i
      (Df.Skip, _) -> satSucc SatWrap i
      (Df.Parallel, _) ->
        -- Index of last sink with data
        fromMaybe maxBound
          $ fold @(sources - 1) (<|>) (zipWith (<$) indicesI fwds)

{- |
Routes packets depending on their metadata, using given routing functions.

Data is sent to at most one sink, for which the dispatch function evaluates to
@True@ when applied to the input metadata. If none of the predicates hold, the
input packet is dropped. If more than one of the predicates hold, the sink
that occurs first in the vector is picked.

Sends out packets in the same clock cycle as they are received, this
component has zero latency and runs at full throughput.
-}
packetDispatcherC ::
  forall dataWidth sinks meta dom.
  (HiddenClockResetEnable dom) =>
  (KnownNat sinks) =>
  -- | Dispatch function
  Vec sinks (meta -> Bool) ->
  Circuit
    (PacketStream dom dataWidth meta)
    (Vec sinks (PacketStream dom dataWidth meta))
packetDispatcherC predicates =
  Circuit (B.second unbundle . unbundle . fmap go . bundle . B.second bundle)
 where
  idleOtp = repeat Nothing
  go (Nothing, _) = (PacketStreamS2M False, idleOtp)
  go (Just x, bwds) = case findIndex id (zipWith ($) predicates (pure $ _meta x)) of
    Just i -> (bwds !! i, replace i (Just x) idleOtp)
    Nothing -> (PacketStreamS2M True, idleOtp)

{- |
Routing function for `packetDispatcherC` that matches against values with
an `Eq` instance. Useful to route according to a record field.
-}
routeBy ::
  (Eq a) =>
  (meta -> a) ->
  Vec sinks a ->
  Vec sinks (meta -> Bool)
routeBy f = map $ \x -> (== x) . f
