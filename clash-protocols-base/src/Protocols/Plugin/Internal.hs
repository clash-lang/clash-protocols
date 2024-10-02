{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Protocols.Plugin.Internal where

import Clash.Explicit.Prelude

import Data.Tagged
import GHC.Base (Any)
import Protocols.Plugin.Types

{- | Picked up by "Protocols.Plugin" to process protocol DSL. See
"Protocols.Plugin" for more information.
-}
circuit :: Any
circuit =
  error "'circuit' called: did you forget to enable \"Protocols.Plugin\"?"

{- | Picked up by "Protocols.Plugin" to tie circuits together. See
"Protocols.Plugin" for more information.
-}
(-<) :: Any
(-<) =
  error "(-<) called: did you forget to enable \"Protocols.Plugin\"?"

{- | Convenience type alias. A circuit where all parts are decorated with a
tag, referring to the @a@ and @b@ in its main signature. This is (indirectly)
used by the plugin to help GHC's type inference.
-}
type TaggedCircuitT a b =
  (Tagged a (Fwd a), Tagged b (Bwd b)) ->
  (Tagged a (Bwd a), Tagged b (Fwd b))

-- | Remove tags from a tagged "Circuit", leaving just a "Circuit".
unTaggedCircuit :: TaggedCircuitT a b -> Circuit a b
unTaggedCircuit f = Circuit $ \(aFwd, bBwd) ->
  let (Tagged aBwd, Tagged bFwd) = f (Tagged aFwd, Tagged bBwd)
   in (aBwd, bFwd)

-- | Add tags to a "Circuit", making a "TaggedCircuitT".
taggedCircuit :: Circuit a b -> TaggedCircuitT a b
taggedCircuit (Circuit c) (aFwd, bBwd) =
  let (aBwd, bFwd) = c (unTagged aFwd, unTagged bBwd)
   in (Tagged aBwd, Tagged bFwd)

-- | Convenience pattern for 'unTaggedCircuit' and 'taggedCircuit'.
pattern TaggedCircuit :: TaggedCircuitT a b -> Circuit a b
pattern TaggedCircuit f <- (taggedCircuit -> f)
  where
    TaggedCircuit f = unTaggedCircuit f

{- | Unsafe version of ':>'. Will fail if applied to empty vectors. This is used to
work around spurious incomplete pattern match warnings generated in newer GHC
versions.
-}
pattern (:>!) :: a -> Vec n a -> Vec (n + 1) a
pattern (:>!) x xs <- (\ys -> (head ys, tail ys) -> (x, xs))

{-# COMPLETE (:>!) #-}
infixr 5 :>!
