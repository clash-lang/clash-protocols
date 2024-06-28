{- |
Simple protocol for request-response communication.
The forward channel channel has type @Signal dom (Maybe req)@ and is used to send requests.
The backward channel has type @Signal dom (Maybe resp)@ and is used to send responses.
The protocol must obey the following rules:
* When the forward channel is @Just a@, it must not change until the transaction is completed.
* The forward channel can not depend on the backward channel.
* When the forward channel is @Nothing@, the backward channel may be undefined.
-}
module Protocols.ReqResp where

import qualified Clash.Prelude as C
import Data.Kind (Type)
import Protocols
import Protocols.Internal.Classes
import Prelude as P

{- | For simple request-response protocols. The forward channel is used to send requests
and the backward channel is used to send responses.
Rules:
* When the forward channel is @Just a@, it must not change until the transaction
  is completed.
* The forward channel can not depend on the backward channel.
* When the forward channel is @Nothing@, the backward channel may be undefined.
-}
data ReqResp (dom :: C.Domain) (req :: Type) (resp :: Type)

instance Protocol (ReqResp dom req resp) where
  -- \| Forward channel for ReqResp protocol:
  type Fwd (ReqResp dom req resp) = C.Signal dom (Maybe req)

  -- \| Backward channel for ReqResp protocol:
  type Bwd (ReqResp dom req resp) = C.Signal dom (Maybe resp)

instance IdleCircuit (ReqResp dom req resp) where
  idleFwd _ = pure Nothing
  idleBwd _ = pure Nothing

{- | Force a @Nothing@ on the backward channel and @Nothing@ on the forward
channel if reset is asserted.
-}
forceResetSanity ::
  forall dom req resp.
  (C.HiddenReset dom) =>
  Circuit (ReqResp dom req resp) (ReqResp dom req resp)
forceResetSanity = forceResetSanityGeneric
