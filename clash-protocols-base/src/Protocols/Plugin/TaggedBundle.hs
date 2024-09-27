{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_HADDOCK hide #-}

-- For debugging TH:
-- {-# OPTIONS_GHC -ddump-splices #-}

module Protocols.Plugin.TaggedBundle where

import Clash.Explicit.Prelude

import Protocols.Plugin.Cpp (maxTupleSize)
import Protocols.Plugin.TaggedBundle.TH (taggedBundleTupleInstances)

import Data.Tagged

{- | A bundle class that retains an attached phantom type @t@. I.e., a crossing
between "Tagged" and "Bundle".
-}
class TaggedBundle t a where
  type TaggedUnbundled t a = res | res -> t a
  taggedBundle :: TaggedUnbundled t a -> Tagged t a
  taggedUnbundle :: Tagged t a -> TaggedUnbundled t a

instance TaggedBundle () () where
  type TaggedUnbundled () () = ()
  taggedBundle = Tagged
  taggedUnbundle = unTagged

instance TaggedBundle (Vec n t) (Vec n a) where
  type TaggedUnbundled (Vec n t) (Vec n a) = Vec n (Tagged t a)
  taggedBundle = Tagged . fmap unTagged
  taggedUnbundle = fmap Tagged . unTagged

{- | A convenience pattern that bundles and unbundles. Can be used as an alternative
to using @ViewPatterns@. I.e., the following:

> myFunction (taggedUnbundle -> ..)

can be written as:

> myFunction (TaggedBundle ..)

Is mostly used by "Protocols.Plugin".
-}
pattern TaggedBundle :: (TaggedBundle t a) => TaggedUnbundled t a -> Tagged t a
pattern TaggedBundle a <- (taggedUnbundle -> a)
  where
    TaggedBundle a = taggedBundle a

{-# COMPLETE TaggedBundle #-}

{- | __NB__: The documentation only shows instances up to /3/-tuples. By
default, instances up to and including /12/-tuples will exist. If the flag
@large-tuples@ is set instances up to the GHC imposed limit will exist. The
GHC imposed limit is either 62 or 64 depending on the GHC version.
-}
instance TaggedBundle (t1, t2) (a1, a2) where
  type TaggedUnbundled (t1, t2) (a1, a2) = (Tagged t1 a1, Tagged t2 a2)
  taggedBundle (Tagged a1, Tagged a2) = Tagged (a1, a2)
  taggedUnbundle (Tagged (a1, a2)) = (Tagged a1, Tagged a2)

-- Generate n-tuple instances, where n > 2
taggedBundleTupleInstances maxTupleSize
