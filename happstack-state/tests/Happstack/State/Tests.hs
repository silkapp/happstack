module Happstack.State.Tests (allTests) where

import Happstack.State.Tests.SaverProperties (saverProperties)
import Happstack.State.Tests.CheckpointProperties (checkpointProperties, congestedCheckpoint, runRestoreCongestionKnownFailures)
import Test.HUnit

allTests :: Test
allTests 
    = "happstack-state" ~: 
      [ runRestoreCongestionKnownFailures
      , checkpointProperties
      , congestedCheckpoint
      , saverProperties
      ]


{- List of properties

 * for all operations, memory saver == file saver
 * saverCut is only an optimization
 * checkpoint is only an optimization
 * events that predate the last checkpoint are ignored

-}
