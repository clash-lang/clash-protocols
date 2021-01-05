{-|
Defines the full AXI4 protocol with port names corresponding to the AXI4
specification. Because some fields are defined partially, it's not advised to
use this in Clash designs. Instead, look at (losslessly) converting it to
"Protocols.Axi4.Full" or (losslessy or lossy) converting it to "Protocols.Df".

Note that every individual channel is a DfLike-protocol, but the bundled version
is not.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Protocols.Axi4.Partial.Full
  ( module ReadAddress
  , module ReadData
  , module WriteAddress
  , module WriteData
  , module WriteResponse
  ) where

import Protocols.Axi4.Partial.Full.ReadAddress as ReadAddress
import Protocols.Axi4.Partial.Full.ReadData as ReadData
import Protocols.Axi4.Partial.Full.WriteAddress as WriteAddress
import Protocols.Axi4.Partial.Full.WriteData as WriteData
import Protocols.Axi4.Partial.Full.WriteResponse as WriteResponse
