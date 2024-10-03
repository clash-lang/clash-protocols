{-|
Copyright  :  (C) 2019     , Myrtle Software Ltd,
                  2023     , QBayLogic B.V.,
                  2024     , Google LLC
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Compile-time dependent constants. Inspired by @clash-prelude@'s @Clash.CPP@.
-}

{-# LANGUAGE CPP #-}

{-# OPTIONS_HADDOCK hide #-}

module Protocols.Plugin.Cpp
 ( maxTupleSize
 , haddockOnly
 ) where

#ifndef MAX_TUPLE_SIZE
#ifdef LARGE_TUPLES

import GHC.Settings.Constants (mAX_TUPLE_SIZE)

#define MAX_TUPLE_SIZE (fromIntegral mAX_TUPLE_SIZE)

#else
#ifdef HADDOCK_ONLY
#define MAX_TUPLE_SIZE 3
#else
#define MAX_TUPLE_SIZE 12
#endif
#endif
#endif

maxTupleSize :: Num a => a
maxTupleSize = MAX_TUPLE_SIZE

haddockOnly :: Bool
#ifdef HADDOCK_ONLY
haddockOnly = True
#else
haddockOnly = False
#endif
