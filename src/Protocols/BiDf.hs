{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

{- |
Module      : Protocols.BiDf
Description : A simple request-response protocol.

This module defines `BiDf`, a basic protocol for request-response communication systems based on two `Df` streams.

[@Request stream@]
The request stream is a stream with the same format as `Df`, where the payload forms a request. For each accepted
request, the circuit must produce a response. This response can be delayed by any number of cycles. The request
stream must obey all rules of the `Df` stream.

[@Response stream@]
The response stream is a `Df` that should produce a response for each accepted request. The response can be delayed
by any number of cycles and must obey all rules of the `Df` stream.

Protocol Rules:
* The request stream must obey all rules of the `Df` stream.
* The response stream must obey all rules of the `Df` stream.
-}
module Protocols.BiDf where

import qualified Clash.Prelude as C
import Data.Kind (Type)
import Data.Tuple
import Protocols

import qualified Protocols.Df as Df

-- type BiDf (dom :: C.Domain) (req :: Type) (resp :: Type) = (Df dom req, Reverse (Df dom resp))

-- | A combination of two `Df` streams, one for requests and one for responses.
type BiDf (dom :: C.Domain) (req :: Type) (resp :: Type) =
  (Df dom req, Reverse (Df dom resp))

{- | Receive a request-response circuit of type  `Circuit (Df dom req) (Df dom resp)`
and transform it into a circuit of type `BiDf dom req resp`
-}
toBiDfSub ::
  forall dom req resp.
  Circuit (Df dom req) (Df dom resp) ->
  Circuit (BiDf dom req resp) ()
toBiDfSub (Circuit df) = Circuit ((,()) . df . fst)

{- | Receive a request-response circuit of type  `BiDf dom req resp`
and transform it into a circuit of type `Circuit (Df dom req) (Df dom resp)`
-}
fromBiDfSub ::
  forall dom req resp.
  Circuit (BiDf dom req resp) () ->
  Circuit (Df dom req) (Df dom resp)
fromBiDfSub (Circuit biDf) = Circuit (fst . biDf . (,()))

{- | Receive a request-response circuit of type  `Circuit (Df dom req) (Df dom resp)`
and transform it into a circuit of type `Circuit () (BiDf dom req resp)`
-}
toBiDfMan ::
  forall dom req resp.
  Circuit (Df dom resp) (Df dom req) ->
  Circuit () (BiDf dom req resp)
toBiDfMan (Circuit df) = Circuit (((),) . swap . df . swap . snd)

{- | Receive a request-response circuit of type  `Circuit () (BiDf dom req resp)`
and transform it into a circuit of type `Circuit (Df dom req) (Df dom req)`
-}
fromBiDfMan ::
  forall dom req resp.
  Circuit () (BiDf dom req resp) ->
  Circuit (Df dom resp) (Df dom req)
fromBiDfMan (Circuit biDf) = Circuit (swap . snd . biDf . ((),) . swap)

-- | Prepend a circuit to the `Df dom req` stream of `BiDf dom req resp`
prependReq ::
  forall dom req resp.
  Circuit (Df dom req) (Df dom req) ->
  Circuit (BiDf dom req resp) (BiDf dom req resp)
prependReq df = Circuit biDf
 where
  biDf ((reqDat0, respAck), (reqAck0, respDat)) =
    ((reqAck1, respDat), (reqDat1, respAck))
   where
    (reqAck1, reqDat1) = toSignals df (reqDat0, reqAck0)

-- | Append a circuit to the `Df dom resp` stream of `BiDf dom req resp`
appendResp ::
  forall dom req resp.
  Circuit (Df dom resp) (Df dom resp) ->
  Circuit (BiDf dom req resp) (BiDf dom req resp)
appendResp df = Circuit biDf
 where
  biDf ((reqDat, respAck0), (reqAck, respDat0)) =
    ((reqAck, respDat1), (reqDat, respAck1))
   where
    (respAck1, respDat1) = toSignals df (respDat0, respAck0)

forceResetSanity ::
  forall dom req resp.
  (C.HiddenClockResetEnable dom) =>
  Circuit (BiDf dom req resp) (BiDf dom req resp)
forceResetSanity = prependReq Df.forceResetSanity |> appendResp Df.forceResetSanity

-- Below you'll find some example code that uses the BiDf protocol.
-- We either remove it before merging or we keep it as a reference for future development.

memoryDf :: Circuit (Df dom addr) (Df dom dat)
memoryDf = undefined

{- | Observes the incoming request and starts producing incremental outgoing requests.
As soong as it receives responses from its subordinate, it offers them to its superior
and acknowledges the incoming request.
-}
prefetcher :: Circuit (BiDf dom addr dat) (BiDf dom addr dat)
prefetcher = undefined

type InstructionBus dom addrW = BiDf dom (C.Unsigned addrW) (C.BitVector 32)

cpu ::
  Circuit
    (CSignal dom (C.BitVector n))
    (InstructionBus dom addrW, CSignal dom C.Bit)
cpu = undefined

someExample :: Circuit (CSignal dom (C.BitVector n)) (CSignal dom C.Bit)
someExample = circuit $ \interrupts -> do
  (cpuBus, uart) <- cpu -< interrupts
  toBiDfSub memoryDf <| prefetcher -< cpuBus
  idC -< uart
