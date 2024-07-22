module Tests.Protocols (tests, main) where

import Test.Tasty
import qualified Tests.Protocols.Avalon
import qualified Tests.Protocols.Axi4
import qualified Tests.Protocols.Df
import qualified Tests.Protocols.DfConv
import qualified Tests.Protocols.PacketStream
import qualified Tests.Protocols.Vec
import qualified Tests.Protocols.Wishbone

tests :: TestTree
tests =
  testGroup
    "Protocols"
    [ Tests.Protocols.Df.tests
    , Tests.Protocols.DfConv.tests
    , Tests.Protocols.Avalon.tests
    , Tests.Protocols.Axi4.tests
    , Tests.Protocols.PacketStream.tests
    , Tests.Protocols.Wishbone.tests
    , Tests.Protocols.Vec.tests
    ]

main :: IO ()
main = defaultMain tests
