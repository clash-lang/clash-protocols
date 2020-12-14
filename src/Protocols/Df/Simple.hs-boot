{-# LANGUAGE RoleAnnotations #-}

module Protocols.Df.Simple where

import Data.Kind (Type)
import Clash.Prelude (Domain)

data Dfs (dom :: Domain) (a :: Type)
type role Dfs phantom phantom
