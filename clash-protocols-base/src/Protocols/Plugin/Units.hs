{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

-- For debugging TH:
-- {-# OPTIONS_GHC -ddump-splices #-}

module Protocols.Plugin.Units where

import Clash.Explicit.Prelude

import Protocols.Plugin.Cpp (maxTupleSize)
import Protocols.Plugin.Units.TH (unitsTupleInstances)

{- | Utilities for zero-width types. Is used by "Protocols.Plugin" to drive \"trivial\"
backwards channels.
-}
class Units a where
  -- | Only inhabitant of type @a@.
  units :: a

instance Units () where
  units = ()

instance Units (Signed 0) where
  units = 0

instance Units (Unsigned 0) where
  units = 0

instance Units (BitVector 0) where
  units = 0

instance Units (Index 0) where
  units = 0

instance Units (Index 1) where
  units = 0

instance (Units a) => Units (Signal dom a) where
  units = pure units

instance (Units a, KnownNat n) => Units (Vec n a) where
  units = repeat units

{- | __NB__: The documentation only shows instances up to /3/-tuples. By
default, instances up to and including /12/-tuples will exist. If the flag
@large-tuples@ is set instances up to the GHC imposed limit will exist. The
GHC imposed limit is either 62 or 64 depending on the GHC version.
-}
instance (Units a1, Units a2) => Units (a1, a2) where
  units = (units, units)

-- Generate n-tuple instances, where n > 2
unitsTupleInstances maxTupleSize
