{- | Definitions for the 'ToConst' protocol. Usable to pass constant values in
circuits.
-}
module Protocols.ToConst (
  ToConst,
  ToConstBwd,
  to,
  from,
  toBwd,
  fromBwd,
) where

import Data.Proxy
import Protocols.Plugin (Circuit (..), ToConst, ToConstBwd)

-- | Convert a value to a 'Circuit' that produces a constant value.
to :: a -> Circuit () (ToConst a)
to a = Circuit Proxy Proxy (\_ -> ((), a))

-- | Extract the constant value
from :: Circuit () (ToConst a) -> a
from (Circuit Proxy Proxy f) = snd (f ((), ()))

{- | Convert a value to a 'Circuit' that produces a constant value in the
backward direction.
-}
toBwd :: a -> Circuit (ToConstBwd a) ()
toBwd a = Circuit Proxy Proxy (\_ -> (a, ()))

-- | Extract the constant value from the backward direction.
fromBwd :: Circuit (ToConstBwd a) () -> a
fromBwd (Circuit Proxy Proxy f) = fst (f ((), ()))
