module Protocols.Internal.Types where

import Data.Proxy
import GHC.Base (Type)
import Protocols.Plugin

{- | Idle state of a Circuit. Aims to provide no data for both the forward and
backward direction. Transactions are not acknowledged.
-}
class (Protocol p) => IdleCircuit p where
  idleFwd :: Proxy p -> Fwd (p :: Type)
  idleBwd :: Proxy p -> Bwd (p :: Type)
