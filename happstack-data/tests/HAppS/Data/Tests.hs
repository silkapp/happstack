-- |HUnit tests and QuickQuick properties for HAppS.Util.*
module HAppS.Data.Tests (allTests) where

import HAppS.Data.Tests.HList001 (hlist001)
import HAppS.Data.Tests.HList002 (hlist002)
import HAppS.Data.Tests.Xml001   (xml001)
import HAppS.Data.Tests.Xml002   (xml002)
import HAppS.Data.Tests.Xml003   (xml003)
import HAppS.Data.Tests.HasT001  (hasT001)

import Test.HUnit as HU (Test(..),(~:))

-- |All of the tests for happstack-util should be listed here. 
allTests :: Test
allTests = 
    "happstack-data tests" ~: [hlist001, hlist002, xml001, xml002, xml003, hasT001]
