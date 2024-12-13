{- |
Copyright  :  (C) 2024, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Provides the PacketStream protocol, a simple streaming protocol for transferring packets of data between components.

Apart from the protocol definition, some components, all of which are generic in @dataWidth@, are also provided:

1. Several small utilities such as filtering a stream based on its metadata.
2. Fifos
3. Components which upsize or downsize @dataWidth@
4. Components which read from the stream (depacketizers)
5. Components which write to the stream (packetizers)
6. Components which split and merge a stream based on its metadata
-}
module Protocols.PacketStream (
  module Protocols.PacketStream.Base,

  -- * Fifos
  module Protocols.PacketStream.PacketFifo,
  module Protocols.PacketStream.AsyncFifo,

  -- * Converters
  module Protocols.PacketStream.Converters,

  -- * Depacketizers
  module Protocols.PacketStream.Depacketizers,

  -- * Packetizers
  module Protocols.PacketStream.Packetizers,

  -- * Padding removal
  module Protocols.PacketStream.Padding,

  -- * Routing components
  module Protocols.PacketStream.Routing,
)
where

import Protocols.PacketStream.AsyncFifo
import Protocols.PacketStream.Base
import Protocols.PacketStream.Converters
import Protocols.PacketStream.Depacketizers
import Protocols.PacketStream.PacketFifo
import Protocols.PacketStream.Packetizers
import Protocols.PacketStream.Padding
import Protocols.PacketStream.Routing
