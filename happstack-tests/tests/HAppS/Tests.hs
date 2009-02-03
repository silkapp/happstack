module HAppS.Tests where

import qualified HAppS.Util.Tests       as Util
import qualified HAppS.Data.Tests       as Data
import qualified HAppS.Data.IxSet.Tests as IxSet
import qualified HAppS.State.Tests      as State
import Test.HUnit

allTests :: Test
allTests = ("happstack" ~: [ Util.allTests, Data.allTests, IxSet.allTests, State.allTests ])