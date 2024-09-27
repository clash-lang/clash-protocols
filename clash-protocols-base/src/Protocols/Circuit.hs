{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Protocols.Circuit (
  module Protocols.Internal.Classes,
) where

import Clash.Signal
import Clash.Sized.Vector
import GHC.TypeNats (KnownNat)
import Protocols.Cpp (maxTupleSize)
import Protocols.Internal.TH
import Protocols.Internal.Classes

instance Protocol () where
  type Fwd () = ()
  type Bwd () = ()

{- | __NB__: The documentation only shows instances up to /3/-tuples. By
default, instances up to and including /12/-tuples will exist. If the flag
@large-tuples@ is set instances up to the GHC imposed limit will exist. The
GHC imposed limit is either 62 or 64 depending on the GHC version.
-}
instance Protocol (a, b) where
  type Fwd (a, b) = (Fwd a, Fwd b)
  type Bwd (a, b) = (Bwd a, Bwd b)

-- Generate n-tuple instances, where n > 2
protocolTupleInstances 3 maxTupleSize

instance (KnownNat n) => Protocol (Vec n a) where
  type Fwd (Vec n a) = Vec n (Fwd a)
  type Bwd (Vec n a) = Vec n (Bwd a)

-- XXX: Type families with Signals on LHS are currently broken on Clash:
instance Protocol (CSignal dom a) where
  type Fwd (CSignal dom a) = Signal dom a
  type Bwd (CSignal dom a) = Signal dom ()
