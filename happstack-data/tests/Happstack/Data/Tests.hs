-- |HUnit tests and QuickQuick properties for Happstack.Data.*
module Happstack.Data.Tests (allTests) where

import Happstack.Data.Tests.Xml001   (xml001)
import Happstack.Data.Tests.Xml002   (xml002)
import Happstack.Data.Tests.Xml003   (xml003)

import Test.HUnit as HU (Test(..),(~:))

-- |All of the tests for happstack-data should be listed here. 
allTests :: Test
allTests = 
    "happstack-data tests" ~: [xml001, xml002, xml003]

