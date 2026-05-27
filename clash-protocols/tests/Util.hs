module Util where

-- base
import Unsafe.Coerce (unsafeCoerce)

-- unordered-containers

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap

-- hashable
import Data.Hashable (Hashable)

-- clash-prelude
import Clash.Prelude qualified as C

-- extra
import "extra" Data.List.Extra (transpose)
import "extra" Data.List.Extra qualified as Extra

chunksOf :: forall n. (C.KnownNat n) => [Int] -> C.Vec n [Int]
chunksOf xs = vecFromList (transpose (Extra.chunksOf (C.natToNum @n) xs))

vecFromList :: forall n a. (C.KnownNat n, Monoid a) => [a] -> C.Vec n a
vecFromList as = C.takeI (unsafeCoerce (as <> repeat mempty))

-- | Count the number of times an element occurs in a list
tally :: (Hashable a, Eq a) => [a] -> HashMap a Int
tally = tallyOn id (const 1)

tallyOn :: (Hashable b, Eq b) => (a -> b) -> (a -> Int) -> [a] -> HashMap b Int
tallyOn f g xs = HashMap.fromListWith (+) (zip (map f xs) (map g xs))
