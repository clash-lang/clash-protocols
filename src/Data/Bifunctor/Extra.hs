module Data.Bifunctor.Extra where

import Data.Bifunctor (bimap)
import Data.Tuple (swap)

{- | Same as 'Data.Bifunctor.bimap', but:

  * Specialized on (,)
  * Swaps elements of resulting tuple
-}
swapMap :: (a -> b) -> (c -> d) -> (a, c) -> (d, b)
swapMap f g = swap . bimap f g
