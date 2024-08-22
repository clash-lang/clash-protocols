{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
Provides a packet arbiter and dispatcher, for merging and splitting packet streams.
-}
module Protocols.PacketStream.Routing (
  packetArbiterC,
  ArbiterMode (..),
  packetDispatcherC,
  routeBy,
) where

import Clash.Prelude

import Protocols
import Protocols.PacketStream.Base

import qualified Data.Bifunctor as B
import Data.Maybe

-- | Collect mode for `packetArbiterC`
data ArbiterMode
  = -- | Collect in a round-robin fashion. Fair and cheaper than `Parallel`.
    RoundRobin
  | -- | Check components in parallel. This mode has a higher throughput, but is
    -- biased towards the last source and also slightly more expensive.
    Parallel

-- | Merges multiple packet streams into one, respecting packet boundaries.
packetArbiterC ::
  forall dom p dataWidth meta.
  (HiddenClockResetEnable dom) =>
  (KnownNat p) =>
  (1 <= p) =>
  -- | See `ArbiterMode`
  ArbiterMode ->
  Circuit (Vec p (PacketStream dom dataWidth meta)) (PacketStream dom dataWidth meta)
packetArbiterC mode = Circuit (B.first unbundle . mealyB go (maxBound, True) . B.first bundle)
 where
  go (i, first) (fwds, bwd@(PacketStreamS2M ack)) = ((i', continue), (bwds, fwd))
   where
    bwds = replace i bwd (repeat (PacketStreamS2M False))
    fwd = fwds !! i
    continue = case fwd of
      Nothing -> first -- only switch sources if this is not somewhere inside a packet
      Just (PacketStreamM2S _ (Just _) _ _) -> ack -- switch source once last packet is acknowledged
      _ -> False
    i' = case (mode, continue) of
      (_, False) -> i
      (RoundRobin, _) -> satSucc SatWrap i -- next index
      (Parallel, _) -> fromMaybe maxBound $ fold @(p - 1) (<|>) (zipWith (<$) indicesI fwds) -- index of last sink with data

{- |
Routes packets depending on their metadata, using given routing functions.

Data is sent to at most one sink, for which the dispatch function evaluates to
@True@ when applied to the input metadata. If none of the predicates hold, the
input packet is dropped. If more than one of the predicates hold, the sink
that occcurs first in the vector is picked.

Sends out packets in the same clock cycle as they are received, this
component has zero latency and runs at full throughput.
-}
packetDispatcherC ::
  forall (dom :: Domain) (p :: Nat) (dataWidth :: Nat) (meta :: Type).
  (HiddenClockResetEnable dom) =>
  (KnownNat p) =>
  -- | Dispatch function
  Vec p (meta -> Bool) ->
  Circuit (PacketStream dom dataWidth meta) (Vec p (PacketStream dom dataWidth meta))
packetDispatcherC predicates = Circuit (B.second unbundle . unbundle . fmap go . bundle . B.second bundle)
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
  Vec p a ->
  Vec p (meta -> Bool)
routeBy f = map $ \x -> (== x) . f
