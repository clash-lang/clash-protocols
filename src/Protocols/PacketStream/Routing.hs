{-# LANGUAGE NoImplicitPrelude #-}

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

import Data.Bifunctor (Bifunctor (second))
import qualified Data.Bifunctor as B
import Data.Maybe

-- | Collect mode for `packetArbiterC`
data ArbiterMode
  = -- | Collect in a round-robin fashion. Fair and cheaper than `Parallel`.
    RoundRobin
  | -- | Check components in parallel. This mode has a higher throughput, but is
    -- biased towards the last source and also slightly more expensive.
    Parallel

-- | Collects packets from all sources, respecting packet boundaries.
packetArbiterC ::
  forall dom p n a.
  ( HiddenClockResetEnable dom
  , KnownNat p
  , 1 <= p
  ) =>
  -- | See `ArbiterMode`
  ArbiterMode ->
  Circuit (Vec p (PacketStream dom n a)) (PacketStream dom n a)
packetArbiterC mode = Circuit (B.first unbundle . mealyB go (maxBound, True) . B.first bundle)
 where
  go ::
    (Index p, Bool) ->
    (Vec p (Maybe (PacketStreamM2S n a)), PacketStreamS2M) ->
    ((Index p, Bool), (Vec p PacketStreamS2M, Maybe (PacketStreamM2S n a)))
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

{- | Routes packets depending on their metadata, using given routing functions.

Data is sent to at most one element of the output vector, for which the
dispatch function evaluates to true on the metadata of the input. If none of
the functions evaluate to true, the input is dropped. If more than one of the
predicates are true, the first one is picked.

Sends out packets in the same clock cycle as they are received.
-}
packetDispatcherC ::
  forall (dom :: Domain) (p :: Nat) (n :: Nat) (a :: Type).
  ( HiddenClockResetEnable dom
  , KnownNat p
  ) =>
  -- | Dispatch function. If function at index i returns true for the metaData it
  -- dispatches the current packet to that sink.
  Vec p (a -> Bool) ->
  Circuit (PacketStream dom n a) (Vec p (PacketStream dom n a))
packetDispatcherC fs = Circuit (second unbundle . unbundle . fmap go . bundle . second bundle)
 where
  go (Just x, bwds) = case findIndex id $ zipWith ($) fs (pure $ _meta x) of
    Just i -> (bwds !! i, replace i (Just x) (repeat Nothing))
    _ -> (PacketStreamS2M True, repeat Nothing)
  go _ = (PacketStreamS2M False, repeat Nothing)

{- | Routing function for `packetDispatcherC` that matches against values with
an `Eq` instance. Useful to route according to a record field.
-}
routeBy ::
  (Eq b) =>
  (a -> b) ->
  Vec p b ->
  Vec p (a -> Bool)
routeBy f = fmap $ \x -> (== x) . f
