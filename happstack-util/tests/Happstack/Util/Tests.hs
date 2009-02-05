-- |HUnit tests and QuickQuick properties for Happstack.Util.*
module Happstack.Util.Tests (allTests) where

import Happstack.Util.Common (split)
import Happstack.Util.Testing (qctest)
import Test.HUnit as HU (Test(..),(~:))
import Happstack.Util.Tests.HostAddress

-- |All of the tests for happstack-util should be listed here. 
allTests :: Test
allTests = 
    "happstack-util tests" ~:
      [splitTest
      ,showHostAddressTest
      ,showHostAddressTest
      ]

-- |turn 'splitTest_prop' into an HUnit test with a label
splitTest :: Test
splitTest = "splitTest" ~: qctest splitTest_prop

-- |a QuickCheck property which tests 'split'
splitTest_prop :: Bool -> [Bool] -> Bool
splitTest_prop elem list =
    let (left1, right1) = split (elem ==) list
        (left2, right2) = break (elem ==) list
        right2' =
            case right2 of
              (r:rs) | r == elem -> rs
              _  -> right2
    in
      (left1 == left2) && (right1 == right2')

-- |test showHostAddress against inet_ntoa to ensure that the same results occur
showHostAddressTest :: Test
showHostAddressTest = "showHostAddressTest" ~: qctest propShowHostAddress

-- |test showHostAddress6 against getNameInfo to ensure that the same results occur
showHostAddress6Test :: Test
showHostAddress6Test = "showHostAddress6Test" ~: qctest propShowHostAddress6

