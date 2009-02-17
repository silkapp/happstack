-- |HUnit tests and QuickQuick properties for Happstack-Contrib
module Happstack.Contrib.Tests (allTests) where

import Happstack.Contrib.Tests.HList001 (hlist001)
import Happstack.Contrib.Tests.HList002 (hlist002)
import Happstack.Contrib.Tests.HasT001  (hasT001)

import Test.HUnit as HU (Test(..),(~:),(~?))

-- |All of the tests for happstack-contrib should be listed here. 
allTests :: Test
allTests =
    "happstack-contrib tests" ~: [hlist001, hlist002, hasT001]

