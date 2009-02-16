-- |HUnit tests and QuickQuick properties for Happstack-Contrib
module Happstack.Contrib.Tests (allTests) where

import Test.HUnit as HU (Test(..),(~:),(~?))

-- |All of the tests for happstack-util should be listed here. 
allTests :: Test
allTests = 
    "happstack-server tests" ~: [dummyTest]

dummyTest :: Test
dummyTest = "dummyTest" ~: True ~? "True"