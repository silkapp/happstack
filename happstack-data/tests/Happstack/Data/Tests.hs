-- |HUnit tests and QuickQuick properties for Happstack.Data.*
module Happstack.Data.Tests (allTests) where

import Happstack.Data.Tests.HList001 (hlist001)
import Happstack.Data.Tests.HList002 (hlist002)
import Happstack.Data.Tests.Xml001   (xml001)
import Happstack.Data.Tests.Xml002   (xml002)
import Happstack.Data.Tests.Xml003   (xml003)
import Happstack.Data.Tests.HasT001  (hasT001)

import Test.HUnit as HU (Test(..),(~:))

-- |All of the tests for happstack-data should be listed here. 
allTests :: Test
allTests = 
    "happstack-data tests" ~: [hlist001, hlist002, xml001, xml002, xml003, hasT001]
