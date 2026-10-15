{-# OPTIONS_HADDOCK hide #-}

-- | Internal utilities for testbench components
module Protocols.Experimental.Testbench.Internal (
  simMealy,
  showText,
  monus,
) where

import Clash.Signal.Internal (Signal ((:-)))
import Data.Text (Text)
import Data.Text qualified as Text
import Numeric.Natural (Natural)

{- | A Mealy machine for simulation-only components. Unlike Clash's @mealy@, the
state does not need an 'Clash.XException.NFDataX' instance, and is not affected
by resets or enables. Components handle resets themselves.

The input is matched lazily, so an output that does not depend on the current
input can be produced before that input is known. The state is evaluated to
WHNF every cycle.
-}
simMealy :: (s -> i -> (s, o)) -> s -> Signal dom i -> Signal dom o
simMealy f = go
 where
  go s ~(i :- is) =
    let (s1, o) = f s i
     in o :- (s1 `seq` go s1 is)

-- | 'show' to 'Text'
showText :: (Show a) => a -> Text
showText = Text.pack . show

-- | Subtraction that saturates at zero
monus :: Natural -> Natural -> Natural
monus a b = if a > b then a - b else 0
