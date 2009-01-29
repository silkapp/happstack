-- |HUnit tests and QuickQuick properties for HAppS.Util.*
module HAppS.Util.Tests (allTests) where

import HAppS.Util.Common (split)
import HAppS.Util.Testing (qctest)
import Test.HUnit as HU (Test(..),(~:))

-- |All of the tests for happstack-util should be listed here. 
allTests :: Test
allTests = 
    "HAppS-Utils tests" ~: [ splitTest ]

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

