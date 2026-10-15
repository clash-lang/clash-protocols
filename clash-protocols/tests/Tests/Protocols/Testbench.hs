-- | Tests of, and using, "Protocols.Experimental.Testbench"
module Tests.Protocols.Testbench (tests) where

import Test.Tasty
import Tests.Protocols.Testbench.Coverage qualified
import Tests.Protocols.Testbench.Df.Checks qualified
import Tests.Protocols.Testbench.Run qualified
import Tests.Protocols.Testbench.Scoreboard qualified
import Tests.Protocols.Testbench.Vcd qualified

tests :: TestTree
tests =
  testGroup
    "Testbench"
    [ Tests.Protocols.Testbench.Run.tests
    , Tests.Protocols.Testbench.Vcd.tests
    , Tests.Protocols.Testbench.Coverage.tests
    , Tests.Protocols.Testbench.Scoreboard.tests
    , Tests.Protocols.Testbench.Df.Checks.tests
    ]
