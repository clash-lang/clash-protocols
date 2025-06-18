{- |
Utility functions that operate on lists, but are not part of `Data.List`.
-}
module Data.List.Extra where

import "base" Data.List qualified as L

{- |
Takes elements from a list while the predicate holds, considers up to @window@ elements
since the last element that satisfied the predicate.

>>> takeWhileAnyInWindow 3 Prelude.odd [1, 2, 3, 6, 8, 10, 12]
[1,2,3]
-}
takeWhileAnyInWindow ::
  -- | Number of elements to consider since the last element that satisfied the predicate.
  Int ->
  -- | Function to test each element.
  (a -> Bool) ->
  -- | Input list
  [a] ->
  -- | List of elements that satisfied the predicate. Ends at an element that
  -- satisfies the predicate.
  [a]
takeWhileAnyInWindow wdw predicate = go wdw []
 where
  go 0 _ _ = []
  go cnt acc (x : xs)
    | predicate x = L.reverse (x : acc) <> go wdw [] xs
    | otherwise = go (pred cnt) (x : acc) xs
  go _ _ _ = []
