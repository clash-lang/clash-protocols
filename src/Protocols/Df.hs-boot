{-# LANGUAGE RoleAnnotations #-}

module Protocols.Df where

import Data.Kind (Type)
import Clash.Prelude (Domain)

data Df (dom :: Domain) (meta :: Type) (a :: Type)
type role Df phantom phantom phantom
