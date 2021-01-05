{-|
Defines the full AXI4 protocol with port names corresponding to the AXI4
specification.

Note that every individual channel is a DfLike-protocol, but the bundled version
is not.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Protocols.Axi4.Strict.Full
  ( module ReadAddress
  , module ReadData
  , module WriteAddress
  , module WriteData
  , module WriteResponse
  ) where

import Protocols.Axi4.Strict.Full.ReadAddress as ReadAddress
import Protocols.Axi4.Strict.Full.ReadData as ReadData
import Protocols.Axi4.Strict.Full.WriteAddress as WriteAddress
import Protocols.Axi4.Strict.Full.WriteData as WriteData
import Protocols.Axi4.Strict.Full.WriteResponse as WriteResponse
