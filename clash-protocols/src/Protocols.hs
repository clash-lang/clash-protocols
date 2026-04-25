{- |
See t'Circuit' for documentation. This module is designed to import unqualified,
i.e. using:

@
  import Protocols
@

Definitions of t'Circuit', 'Fwd', 'Bwd', and 'Protocols.Df.Df', inspired by
definitions in @circuit-notation@ at <https://github.com/cchalmers/circuit-notation>.
-}
module Protocols (
  -- * Circuit definition
  Circuit (Circuit),
  Protocol (Fwd, Bwd),
  Ack (..),
  Reverse,

  -- * Combinators & functions
  (|>),
  (<|),
  fromSignals,
  toSignals,

  -- * Protocol types
  CSignal,
  Df,
  ToConst,
  ToConstBwd,

  -- * Basic circuits
  idC,
  repeatC,
  applyC,
  prod2C,

  -- * Circuit notation plugin
  circuit,
  (-<),
  Units (..),
  TaggedBundle (..),
) where

import Protocols.Df (Df)
import Protocols.Internal
