module Main (main) where

import SaverProperties (checkSaverProperties)
import CheckpointProperties (testCongestedCheckpoint
                            ,checkCheckpointProperties)

main :: IO ()
main = do checkSaverProperties
          checkCheckpointProperties
          testCongestedCheckpoint


{- List of properties

 * for all operations, memory saver == file saver
 * saverCut is only an optimization
 * checkpoint is only an optimization
 * events that predate the last checkpoint are ignored

-}
