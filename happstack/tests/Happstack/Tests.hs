module Happstack.Tests where

import qualified Happstack.Util.Tests       as Util
import qualified Happstack.Data.Tests       as Data
import qualified Happstack.Data.IxSet.Tests as IxSet
import qualified Happstack.State.Tests      as State
import qualified Happstack.Server.Tests     as Server
import qualified Happstack.Server.Tests     as Contrib
import Test.HUnit

allTests :: Test
allTests = ("happstack" ~: [ Util.allTests, Data.allTests, IxSet.allTests, State.allTests, Server.allTests, Contrib.allTests ])