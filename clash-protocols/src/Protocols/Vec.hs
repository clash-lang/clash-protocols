-- | Utility functions for working with `Vec`s of `Circuit`s.
module Protocols.Vec (
  vecCircuits,
  append,
  append3,
  split,
  split3,
  zip,
  zip3,
  zip4,
  zip5,
  unzip,
  unzip3,
  concat,
  unconcat,
) where

-- base
import Data.Tuple
import Prelude ()

-- clash-prelude
import Clash.Prelude hiding (
  concat,
  split,
  unconcat,
  unzip,
  unzip3,
  zip,
  zip3,
  zip4,
  zip5,
 )
import Clash.Prelude qualified as C

-- clash-protocols-base
import Protocols.Plugin

{- | "Bundle" together a 'Vec' of 'Circuit's into a 'Circuit' with 'Vec' input and output.
The 'Circuit's all run in parallel.

The inverse of 'vecCircuits' can not exist, as we can not guarantee that that the @n@th
manager interface only depends on the @n@th subordinate interface.
-}
vecCircuits :: (C.KnownNat n) => C.Vec n (Circuit a b) -> Circuit (C.Vec n a) (C.Vec n b)
vecCircuits fs = Circuit (\inps -> C.unzip $ f <$> fs <*> uncurry C.zip inps)
 where
  f (Circuit ff) = ff

{- | Applies the mappings @Fwd a -> Fwd b@ and @Bwd b -> Bwd a@ to the circuit's signals.

The idea here is that you want to treat some @a -> b@ as a @Circuit a b@, but @Circuit a b@ is
actually the type @(Fwd a, Bwd b) -> (Bwd a, Fwd b)@. To bridge this gap, we say that @a -> b@ can
be our map from @Fwd a -> Fwd b@, but then we need to fill in the @Bwd b -> Bwd a@ still. In almost
all cases, the former is the function you want to apply, and the latter is the inverse. For
instance, the '(++)' operator on vectors can be made into a @Circuit a b@ with
@applyC (uncurry (++)) splitAtI@, since 'splitAtI' is the inverse of '(++)'.
-}
applyC ::
  forall a b.
  (Fwd a -> Fwd b) ->
  (Bwd b -> Bwd a) ->
  Circuit a b
applyC fwdFn bwdFn = Circuit go
 where
  go :: (Fwd a, Bwd b) -> (Bwd a, Fwd b)
  go (fwdA, bwdB) = (bwdFn bwdB, fwdFn fwdA)

-- | Append two separate vectors of the same circuits into one vector of circuits
append ::
  (C.KnownNat n0) =>
  Circuit (C.Vec n0 circuit, C.Vec n1 circuit) (C.Vec (n0 + n1) circuit)
append = applyC (uncurry (++)) splitAtI

-- | Append three separate vectors of the same circuits into one vector of circuits
append3 ::
  (C.KnownNat n0, C.KnownNat n1, KnownNat n2) =>
  Circuit
    (C.Vec n0 circuit, C.Vec n1 circuit, C.Vec n2 circuit)
    (C.Vec (n0 + n1 + n2) circuit)
append3 = applyC (uncurry3 append3Vec) split3Vec

-- | Split a vector of circuits into two vectors of circuits.
split ::
  (C.KnownNat n0) =>
  Circuit (C.Vec (n0 + n1) circuit) (C.Vec n0 circuit, C.Vec n1 circuit)
split = applyC splitAtI (uncurry (++))

-- | Split a vector of circuits into three vectors of circuits.
split3 ::
  (C.KnownNat n0, C.KnownNat n1, C.KnownNat n2) =>
  Circuit
    (C.Vec (n0 + n1 + n2) circuit)
    (C.Vec n0 circuit, C.Vec n1 circuit, C.Vec n2 circuit)
split3 = applyC split3Vec (uncurry3 append3Vec)

{- | Transforms two vectors of circuits into a vector of tuples of circuits.
Only works if the two vectors have the same length.
-}
zip ::
  (C.KnownNat n) =>
  Circuit (C.Vec n a, C.Vec n b) (C.Vec n (a, b))
zip = applyC (uncurry C.zip) C.unzip

{- | Transforms three vectors of circuits into a vector of tuples of circuits.
Only works if the three vectors have the same length.
-}
zip3 ::
  (C.KnownNat n) =>
  Circuit (C.Vec n a, C.Vec n b, C.Vec n c) (C.Vec n (a, b, c))
zip3 = applyC (uncurry3 C.zip3) C.unzip3

{- | Transforms four vectors of circuits into a vector of tuples of circuits.
Only works if the four vectors have the same length.
-}
zip4 ::
  (C.KnownNat n) =>
  Circuit (C.Vec n a, C.Vec n b, C.Vec n c, C.Vec n d) (C.Vec n (a, b, c, d))
zip4 = applyC (\(a, b, c, d) -> C.zip4 a b c d) unzip4

{- | Transforms five vectors of circuits into a vector of tuples of circuits.
Only works if the five vectors have the same length.
-}
zip5 ::
  (C.KnownNat n) =>
  Circuit (C.Vec n a, C.Vec n b, C.Vec n c, C.Vec n d, C.Vec n e) (C.Vec n (a, b, c, d, e))
zip5 = applyC (\(a, b, c, d, e) -> C.zip5 a b c d e) unzip5

-- | Unzip a vector of tuples of circuits into a tuple of vectors of circuits.
unzip ::
  (C.KnownNat n) =>
  Circuit (C.Vec n (a, b)) (C.Vec n a, C.Vec n b)
unzip = applyC C.unzip (uncurry C.zip)

-- | Unzip a vector of 3-tuples of circuits into a 3-tuple of vectors of circuits.
unzip3 ::
  (C.KnownNat n) =>
  Circuit (C.Vec n (a, b, c)) (C.Vec n a, C.Vec n b, C.Vec n c)
unzip3 = applyC C.unzip3 (uncurry3 C.zip3)

-- | transform a vector of vectors of circuits into a vector of circuits.
concat ::
  (C.KnownNat n0, C.KnownNat n1) =>
  Circuit (C.Vec n0 (C.Vec n1 circuit)) (C.Vec (n0 * n1) circuit)
concat = applyC C.concat (C.unconcat SNat)

-- | transform a vector of circuits into a vector of vectors of circuits.
unconcat ::
  (C.KnownNat n, C.KnownNat m) =>
  SNat m ->
  Circuit (C.Vec (n * m) circuit) (C.Vec n (C.Vec m circuit))
unconcat SNat = applyC (C.unconcat SNat) C.concat

-- Internal utilities

-- | Uncurry a function with three arguments into a function that takes a 3-tuple as argument.
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

-- Append three vectors of `a` into one vector of `a`.
append3Vec ::
  (KnownNat n0, KnownNat n1, KnownNat n2) =>
  C.Vec n0 a ->
  C.Vec n1 a ->
  C.Vec n2 a ->
  C.Vec (n0 + n1 + n2) a
append3Vec v0 v1 v2 = v0 ++ v1 ++ v2

-- Split a C.Vector of 3-tuples into three vectors of the same length.
split3Vec ::
  (KnownNat n0, KnownNat n1, KnownNat n2) =>
  C.Vec (n0 + n1 + n2) a ->
  (C.Vec n0 a, C.Vec n1 a, C.Vec n2 a)
split3Vec v = (v0, v1, v2)
 where
  (v0, splitAtI -> (v1, v2)) = splitAtI v
