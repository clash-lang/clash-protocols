{-# LANGUAGE NoImplicitPrelude #-}

module Clash.Sized.Vector.Extra (
  dropLe,
  takeLe,
  appendVec,
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

-- | Take the first 'valid' elements of 'xs', append 'ys', then pad with 0s
appendVec ::
  forall n m a.
  (KnownNat n) =>
  (Num a) =>
  Index n ->
  Vec n a ->
  Vec m a ->
  Vec (n + m) a
appendVec valid xs ys = results !! valid
 where
  go :: forall l. SNat l -> Vec (n + m) a
  go l@SNat =
    let f = addSNat l d1
     in case compareSNat f (SNat @n) of
          SNatLE -> takeLe (addSNat l d1) xs ++ ys ++ extra
           where
            extra :: Vec (n - (l + 1)) a
            extra = repeat 0
          _ -> error "appendVec: Absurd"
  results = smap (\s _ -> go s) xs
