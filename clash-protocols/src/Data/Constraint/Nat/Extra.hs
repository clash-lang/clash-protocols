{-# LANGUAGE AllowAmbiguousTypes #-}

{-
NOTE [constraint solver addition]

The functions in this module enable us introduce trivial constraints that are not
solved by the constraint solver.
-}
module Data.Constraint.Nat.Extra where

import Clash.Prelude
import Data.Constraint
import Unsafe.Coerce (unsafeCoerce)

-- | if (1 <= b) then (Mod a b + 1 <= b)
leModulusDivisor :: forall a b. (1 <= b) => Dict (Mod a b + 1 <= b)
leModulusDivisor = unsafeCoerce (Dict :: Dict (0 <= 0))

-- | if (1 <= a) and (1 <= b) then (1 <= DivRU a b)
strictlyPositiveDivRu ::
  forall a b.
  (1 <= a, 1 <= b) =>
  -- | XXX: The 'Constraint' equality is there to work around
  -- issue github.com/clash-lang/ghc-typelits-extra/issues/68.
  Dict ((1 <= DivRU a b) ~ (() :: Constraint))
strictlyPositiveDivRu = unsafeCoerce (Dict :: Dict (() ~ ()))

-- | if (1 <= a) then (b <= ceil(b/a) * a)
leTimesDivRu :: forall a b. (1 <= a) => Dict (b <= a * DivRU b a)
leTimesDivRu = unsafeCoerce (Dict :: Dict (0 <= 0))

-- | if (1 <= a) then (a * ceil(b/a) ~ b + Mod (a - Mod b a) a)
eqTimesDivRu :: forall a b. (1 <= a) => Dict (a * DivRU b a ~ b + Mod (a - Mod b a) a)
eqTimesDivRu = unsafeCoerce (Dict :: Dict (0 ~ 0))
