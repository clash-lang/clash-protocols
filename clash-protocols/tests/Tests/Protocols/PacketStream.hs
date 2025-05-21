module Tests.Protocols.PacketStream (tests) where

import Test.Tasty

import Tests.Protocols.PacketStream.AsyncFifo qualified
import Tests.Protocols.PacketStream.Base qualified
import Tests.Protocols.PacketStream.Converters qualified
import Tests.Protocols.PacketStream.Depacketizers qualified
import Tests.Protocols.PacketStream.PacketFifo qualified
import Tests.Protocols.PacketStream.Packetizers qualified
import Tests.Protocols.PacketStream.Padding qualified
import Tests.Protocols.PacketStream.Routing qualified

tests :: TestTree
tests =
  testGroup
    "PacketStream"
    [ Tests.Protocols.PacketStream.AsyncFifo.tests
    , Tests.Protocols.PacketStream.Base.tests
    , Tests.Protocols.PacketStream.Converters.tests
    , Tests.Protocols.PacketStream.Depacketizers.tests
    , Tests.Protocols.PacketStream.PacketFifo.tests
    , Tests.Protocols.PacketStream.Packetizers.tests
    , Tests.Protocols.PacketStream.Padding.tests
    , Tests.Protocols.PacketStream.Routing.tests
    ]
