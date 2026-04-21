-- SPDX-FileCopyrightText: 2025 Google LLC
--
-- SPDX-License-Identifier: Apache-2.0
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}

-- | Version of `Functor` for `Circuit`s.
module Protocols.FunctorC (
  FunctorC (..),
) where

import Clash.Prelude
import Protocols

-- | Version of `Functor` for `Circuit`s.
class FunctorC p where
  fmapC :: Circuit a b -> Circuit (p a) (p b)

instance FunctorC ((,) a) where
  fmapC f = circuit $ \(a, b) -> do
    b' <- f -< b
    idC -< (a, b')

instance FunctorC (Vec n) where
  fmapC = repeatC
