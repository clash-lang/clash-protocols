-- SPDX-FileCopyrightText: 2026 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}

{- |
Contains the `ReqResp` protocol, the simplest possible protocol for request-response communication
together with utilities for working with it. `ReqResp` is suitable for simple request-response
interactions where pipelining is not required. If you need pipelining, you can use the `BiDf`
protocol instead.
-}
module Protocols.ReqResp (
  -- * Types
  ReqResp,

  -- * Converters
  fromDfs,
  toDfs,
  fromBiDf,
  toBiDf,
  requests,
  dropResponse,

  -- * Other utilities
  partitionEithers,
  forceResetSanity,
) where

import Clash.Prelude

import Data.Bifunctor (Bifunctor (..))
import Data.Maybe
import Protocols
import Protocols.BiDf (BiDf)
import Protocols.Idle

import Clash.Explicit.Prelude qualified as E
import Clash.Prelude qualified as C
import Protocols qualified as Df
import Protocols.BiDf qualified as BiDf

{- |
Simplest possible protocol for request-response communication.

The forward channel channel has type @Signal dom (Maybe req)@ and is used to send requests and
the backward channel has type @Signal dom (Maybe resp)@ and is used to send responses. This protocol
can not be pipelined because the request is acknowledged by the response. If you wish to utilize
pipelining, you can use the `BiDf` protocol instead.

The protocol must obey the following rules:
* When the forward channel is @Just a@, it must not change until the transaction is completed.
* The forward channel can not depend on the backward channel.
* When the forward channel is @Nothing@, the backward channel must not be observed.
-}
data ReqResp (dom :: C.Domain) (req :: Type) (resp :: Type)

instance Protocol (ReqResp dom req resp) where
  -- \| Request channel for ReqResp protocol
  type Fwd (ReqResp dom req resp) = C.Signal dom (Maybe req)

  -- \| Response channel for ReqResp protocol
  type Bwd (ReqResp dom req resp) = C.Signal dom (Maybe resp)

instance IdleCircuit (ReqResp dom req resp) where
  idleFwd _ = pure Nothing
  idleBwd _ = pure Nothing

instance
  ( C.KnownDomain dom
  , C.NFDataX req
  , C.NFDataX resp
  , Show req
  , Show resp
  , ShowX req
  , ShowX resp
  ) =>
  Simulate (ReqResp dom req resp)
  where
  type SimulateFwdType (ReqResp dom req resp) = [Maybe req]
  type SimulateBwdType (ReqResp dom req resp) = [Maybe resp]
  type SimulateChannels (ReqResp dom req resp) = 1
  simToSigFwd _ = C.fromList_lazy
  simToSigBwd _ = C.fromList_lazy
  sigToSimFwd _ = E.sample_lazy
  sigToSimBwd _ = E.sample_lazy
  stallC conf stalls = C.withClockResetEnable clockGen resetGen enableGen $ circuit $ \rr0 -> do
    reqs0 <- toDfs -< (rr0, resps)
    reqs1 <- Df.stallC conf stalls -< reqs0
    (rr1, resps) <- fromDfs -< reqs1
    idC -< rr1

leftToMaybe :: Either a b -> Maybe a
leftToMaybe (Left x) = Just x
leftToMaybe (Right _) = Nothing
rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left _) = Nothing
rightToMaybe (Right y) = Just y

-- | Partition a `ReqResp` with an `Either` request type into two `ReqResp`s, one for each side of the `Either`.
partitionEithers ::
  forall dom a b c. Circuit (ReqResp dom (Either a b) c) (ReqResp dom a c, ReqResp dom b c)
partitionEithers = Circuit goS
 where
  goS (eitherFwd, (leftBwd, rightBwd)) = (eitherBwd, (leftFwd, rightFwd))
   where
    leftFwd = fmap (>>= leftToMaybe) eitherFwd
    rightFwd = fmap (>>= rightToMaybe) eitherFwd

    eitherBwd = selectBwd <$> eitherFwd <*> leftBwd <*> rightBwd
  selectBwd (Just (Left _)) leftBwd _ = leftBwd
  selectBwd (Just (Right _)) _ rightBwd = rightBwd
  selectBwd Nothing _ _ = Nothing

{- | Forces a constant response on the backward channel. Useful for when you don't care
about the response data.
-}
dropResponse :: resp -> Circuit (ReqResp dom req resp) (ReqResp dom req ())
dropResponse resp = applyC id (fmap $ fmap $ const resp)

{- | Force a @Nothing@ on the backward channel and @Nothing@ on the forward
channel if reset is asserted.
-}
forceResetSanity ::
  forall dom req resp.
  (C.HiddenReset dom) =>
  Circuit (ReqResp dom req resp) (ReqResp dom req resp)
forceResetSanity = forceResetSanityGeneric

-- | Convert a `ReqResp` protocol to two `Df` streams, one for requests and one for responses.
toDfs ::
  forall dom req resp.
  (C.HiddenClockResetEnable dom) =>
  Circuit (ReqResp dom req resp, Df dom resp) (Df dom req)
toDfs = ckt
 where
  ckt =
    Circuit (first C.unbundle . C.unbundle . C.mealy go Nothing . C.bundle . first C.bundle)
  go Nothing _ = (Just False, ((Nothing, Ack False), Nothing))
  go (Just accepted0) ~(~(reqLeft, resp), ~(Ack reqRightAck)) = (Just accepted1, ((resp, respAck), reqRight))
   where
    respAck = Ack True

    reqRight
      | accepted0 = Nothing
      | otherwise = reqLeft

    accepted1
      | isNothing reqLeft = False -- No request to accept
      | isJust resp = False -- Receiving a response clears the state
      | isJust reqRight = reqRightAck -- A request for which we have not received a response yet
      | otherwise = accepted0

-- | Convert two `Df` streams for requests and responses into a `ReqResp` protocol.
fromDfs ::
  forall dom req resp.
  (C.HiddenClockResetEnable dom, C.NFDataX resp) =>
  Circuit (Df dom req) (ReqResp dom req resp, Df dom resp)
fromDfs =
  Circuit (second C.unbundle . C.unbundle . C.mealy go Nothing . C.bundle . second C.bundle)
 where
  go Nothing ~(req, ~(resp, Ack ack)) = (nextState, (Ack (isJust resp), (req, resp)))
   where
    nextState
      | isJust resp && not ack = resp
      | otherwise = Nothing
  go stored (_, (_, Ack ack)) = (nextState, (Ack False, (Nothing, stored)))
   where
    nextState
      | ack = Nothing
      | otherwise = stored

-- | Convert a `ReqResp` protocol to a `BiDf` protocol through `toDfs` and `BiDf.fromDfs`.
toBiDf ::
  forall dom req resp.
  (C.HiddenClockResetEnable dom) =>
  Circuit (ReqResp dom req resp) (BiDf dom req resp)
toBiDf = circuit $ \reqresp -> do
  request <- toDfs -< (reqresp, response)
  (biDf, response) <- BiDf.fromDfs -< request
  idC -< biDf

-- | Convert a `BiDf` protocol to a `ReqResp` protocol through `fromDfs` and `BiDf.toDfs`.
fromBiDf ::
  forall dom req resp.
  (C.HiddenClockResetEnable dom, C.NFDataX resp) =>
  Circuit (BiDf dom req resp) (ReqResp dom req resp)
fromBiDf = circuit $ \biDf -> do
  request <- BiDf.toDfs -< (biDf, response)
  (reqresp, response) <- fromDfs -< request
  idC -< reqresp

-- | Convert a 'ReqResp' protocol where the response type is '()' to a 'Df' stream of requests.
requests ::
  forall dom req.
  (C.KnownDomain dom) =>
  Circuit (ReqResp dom req ()) (Df dom req)
requests = Circuit (C.unbundle . fmap go . C.bundle)
 where
  go ~(request, Ack ack) = (if ack then Just () else Nothing, request)
