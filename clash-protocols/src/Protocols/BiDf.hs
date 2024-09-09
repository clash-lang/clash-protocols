{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}

-- | Bi-directional request/response-style 'Df' channels.
module Protocols.BiDf (
  BiDf,
    -- * Conversion
  fromDfs,
  toDfs,
  fromBiDf,
  toBiDf,
    -- * Trivial combinators
  void,
  loopback,
    -- * Mapping
  dimap,
  -- * Fan-in
  fanin
) where

import Prelude ()

import Clash.Prelude

import Protocols
import qualified Protocols.Df as Df

-- | A 'Protocol' allowing requests to be passed downstream, with corresponding
-- responses being passed back upstream. Responses are provided in the order that
-- their corresponding requests were submitted.
--
-- *Correctness conditions*
--
--  - The response channel must not produce a value before the request channel
--    has produced a value.
--
--  - Each request must be paired with exactly one response.
--
--  - Responses must be issued in the order that their corresponding requests arrived.
--
--  - Both the request and response channels must obey usual 'Df' correctness
--    conditions.
--
--  - There must not be a combinational path from the request channel to the
--    response channel.
--
type BiDf dom req resp =
  (Df dom req, Reverse (Df dom resp))

-- | Convert a circuit of 'Df's to a 'BiDf' circuit.
toBiDf
  :: Circuit (Df dom req) (Df dom resp)
  -> Circuit (BiDf dom req resp) ()
toBiDf c = circuit $ \bidf -> do
  resp <- c -< req
  req <- toDfs -< (bidf, resp)
  idC -< ()

-- | Convert a 'BiDf' circuit to a circuit of 'Df's.
fromBiDf
  :: Circuit (BiDf dom req resp) ()
  -> Circuit (Df dom req) (Df dom resp)
fromBiDf c = circuit $ \req -> do
  (biDf, resp) <- fromDfs -< req
  c -< biDf
  idC -< resp

-- | Convert a pair of a request and response 'Df`s into a 'BiDf'.
toDfs :: Circuit (BiDf dom req resp, Df dom resp) (Df dom req)
toDfs = fromSignals $ \(~((reqData, respAck), respData), reqAck) ->
  (((reqAck, respData), respAck), reqData)

-- | Convert a 'BiDf' into a pair of request and response 'Df`s.
fromDfs :: Circuit (Df dom req) (BiDf dom req resp, Df dom resp)
fromDfs = fromSignals $ \(reqData, ~((reqAck, respData), respAck)) ->
  (reqAck, ((reqData, respAck), respData))

-- | Ignore all requests, never providing responses.
void :: (HiddenClockResetEnable dom) => Circuit (BiDf dom req resp') ()
void = circuit $ \biDf -> do
  req <- toDfs -< (biDf, resp)
  resp <- Df.empty -< ()
  Df.void -< req

-- | Return mapped requests as responses.
loopback
  :: (HiddenClockResetEnable dom, NFDataX req)
  => (req -> resp)
  -> Circuit (BiDf dom req resp) ()
loopback f = circuit $ \biDf -> do
  req <- toDfs -< (biDf, resp)
  resp <- Df.map f <| Df.registerFwd -< req
  idC -< ()

-- | Map both requests and responses.
dimap
  :: (req -> req')
  -> (resp -> resp')
  -> Circuit (BiDf dom req resp') (BiDf dom req' resp)
dimap f g = circuit $ \biDf -> do
  req <- toDfs -< (biDf, resp')
  req' <- Df.map f -< req
  resp' <- Df.map g -< resp
  (biDf', resp) <- fromDfs -< req'
  idC -< biDf'

-- | Merge a number of 'BiDf's, preferring requests from the last channel.
fanin
  :: forall n dom req resp.
     ( KnownNat n
     , 1 <= n
     , NFDataX req
     , NFDataX resp
     , HiddenClockResetEnable dom
     )
  => Circuit (Vec n (BiDf dom req resp)) (BiDf dom req resp)
fanin = fromSignals $ \(upFwds, (reqAck, respData)) ->
  let reqDatas :: Vec n (Signal dom (Maybe req))
      reqDatas = map fst upFwds
      respAcks :: Vec n (Signal dom Ack)
      respAcks = map snd upFwds

      ((reqAcks, respAck), (respDatas, reqData)) =
           toSignals fanin' ((reqDatas, respData), (respAcks, reqAck))
  in (zip reqAcks respDatas, (reqData, respAck))
 where
  fanin'
      :: Circuit (Vec n (Df dom req), Df dom resp)
                 (Vec n (Df dom resp), Df dom req)
  fanin' = circuit $ \(reqs, resp) -> do
    [fwd0, fwd1]
        <- Df.fanout
        <| Df.roundrobinCollect @n Df.Skip
        <| repeatWithIndexC (\i -> Df.map (\x -> (i,x)))
        -< reqs

    activeN <- Df.map fst -< fwd1
    resps <- Df.route <| Df.zip -< (activeN, resp)
    req <- Df.map snd -< fwd0
    idC -< (resps, req)
