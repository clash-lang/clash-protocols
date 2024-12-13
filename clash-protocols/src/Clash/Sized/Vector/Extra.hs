{-# LANGUAGE NoImplicitPrelude #-}

module Clash.Sized.Vector.Extra (
  dropLe,
  takeLe,
) where

import Clash.Prelude

-- | Like 'drop' but uses a 'Data.Type.Ord.<=' constraint
dropLe ::
  forall
    (n :: Nat)
    (m :: Nat)
    (a :: Type).
  (n <= m) =>
  -- | How many elements to take
  SNat n ->
  -- | input vector
  Vec m a ->
  Vec (m - n) a
dropLe SNat vs = leToPlus @n @m $ dropI vs

-- | Like 'take' but uses a 'Data.Type.Ord.<=' constraint
takeLe ::
  forall
    (n :: Nat)
    (m :: Nat)
    (a :: Type).
  (n <= m) =>
  -- | How many elements to take
  SNat n ->
  -- | input vector
  Vec m a ->
  Vec n a
takeLe SNat vs = leToPlus @n @m $ takeI vs
