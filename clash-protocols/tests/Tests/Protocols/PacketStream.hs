module Tests.Protocols.PacketStream (tests) where

import Test.Tasty

import qualified Tests.Protocols.PacketStream.AsyncFifo
import qualified Tests.Protocols.PacketStream.Converters
import qualified Tests.Protocols.PacketStream.PacketFifo
import qualified Tests.Protocols.PacketStream.Packetizers
import qualified Tests.Protocols.PacketStream.Routing

tests :: TestTree
tests =
  testGroup
    "PacketStream"
    [ Tests.Protocols.PacketStream.AsyncFifo.tests
    , Tests.Protocols.PacketStream.Converters.tests
    , Tests.Protocols.PacketStream.PacketFifo.tests
    , Tests.Protocols.PacketStream.Packetizers.tests
    , Tests.Protocols.PacketStream.Routing.tests
    ]
