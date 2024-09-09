module Tests.Protocols (tests, main) where

import Test.Tasty
import Tests.Protocols.Avalon qualified
import Tests.Protocols.Axi4 qualified
import Tests.Protocols.BiDf qualified
import Tests.Protocols.Df qualified
import Tests.Protocols.DfConv qualified
import Tests.Protocols.PacketStream qualified
import Tests.Protocols.Vec qualified
import Tests.Protocols.Wishbone qualified

tests :: TestTree
tests =
  testGroup
    "Protocols"
    [ Tests.Protocols.BiDf.tests
    , Tests.Protocols.Df.tests
    , Tests.Protocols.DfConv.tests
    , Tests.Protocols.Avalon.tests
    , Tests.Protocols.Axi4.tests
    , Tests.Protocols.PacketStream.tests
    , Tests.Protocols.Wishbone.tests
    , Tests.Protocols.Vec.tests
    ]

main :: IO ()
main = defaultMain tests
