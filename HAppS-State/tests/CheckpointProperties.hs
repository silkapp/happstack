{-# OPTIONS -fglasgow-exts -fth #-}
module CheckpointProperties
    ( testCongestedCheckpoint
    , checkCheckpointProperties
    ) where

import HAppS.State
import HAppS.State.ComponentTH
import HAppS.State.TxControl

import Helpers

import Control.Monad
import Control.Monad.State (get,put)
import Control.Monad.Reader (ask)
import Control.Exception
import Control.Concurrent
import Data.Typeable
import Text.Printf

import Test.QuickCheck
import Test.QuickCheck.Batch

--------------------------------------------------------------
-- Checkpoint congestion
--------------------------------------------------------------

data RestoreTest = RestoreTest Int deriving (Typeable)
instance Version RestoreTest
$(deriveSerialize ''RestoreTest)

data Block = Block (Maybe (MVar ())) deriving (Typeable)

-- We don't want to block events that are loaded from disk.
instance Version Block
instance Serialize Block where
    putCopy _ = contain $ return ()
    getCopy = contain $ return $ Block Nothing

succValue :: Block -> Update RestoreTest ()
succValue block
    = do RestoreTest n <- get
         put $ RestoreTest (n+1)
         case block of
           Block (Just mv) -> unsafeIOToEv $ takeMVar mv
           _               -> return ()

getValue :: Query RestoreTest Int
getValue = do RestoreTest n <- ask
              return n

$(mkMethods ''RestoreTest [ 'succValue
                          , 'getValue])

instance Component RestoreTest where
    type Dependencies RestoreTest = End
    initialValue = RestoreTest 0

restoreTestEntryPoint :: Proxy RestoreTest
restoreTestEntryPoint = Proxy

{-
  There is a small window after the saver has been cut
  and the checkpoints have been queried. Events that are
  executed in this windows must be ignored when the state
  is restored. That is, events with a transaction id lower
  than the saved checkpoint should not be replayed.
  In applications, this is bound to happen when the processing
  queue is congested (non-empty).

  We will trigger this occasion via a blocking event. The event
  will enter the processing queue and block before it has been written
  to disk. We will then schedule a checkpoint and unblock the event.

  The event will be written in the same file as the checkpoint. If
  the event isn't discarded when the state is restored, the value
  in the state will be '2' instead of '1'.
-}
testCongestedCheckpoint
    = withMemorySaver $ \saver ->
      do initCongestedCheckpoint (Queue saver)
         checkRestoredValue (Queue saver)

checkRestoredValue saver
    = bracket (runTxSystem saver restoreTestEntryPoint) (shutdownSystem) $ \ctl ->
      do val <- query $ GetValue
         when (val /= 1) $ error $ "testCongestedCheckpoint failed: " ++ show val
         printf "%25s : OK\n" "Checkpoint congestion"

initCongestedCheckpoint saver
    = bracket (runTxSystem saver restoreTestEntryPoint) (shutdownSystem) $ \ctl ->
      do mv <- newEmptyMVar
         forkIO $ do update $ SuccValue (Block (Just mv))
         yield -- Relying on yields for correctness isn't a great idea.
         forkIO $ createCheckpoint ctl
         yield -- However, it's OK for now since tests aren't vital.
         putMVar mv ()

--------------------------------------------------------------
-- Run/Restore
--------------------------------------------------------------

data RunRestore = RunRestore [Int] deriving (Typeable,Show)
instance Version RunRestore
$(deriveSerialize ''RunRestore)

push :: Int -> Update RunRestore ()
push val = do RunRestore ls <- get
              put $ RunRestore (val:ls)

peek :: Query RunRestore [Int]
peek = do RunRestore ls <- ask
          return $ reverse ls

$(mkMethods ''RunRestore [ 'push
                         , 'peek])

instance Component RunRestore where
    type Dependencies RunRestore = End
    initialValue = RunRestore []

runRestoreEntryPoint :: Proxy RunRestore
runRestoreEntryPoint = Proxy

genRunRestoreProp withSaver check action
    = ioProperty $ withSaver $ \saver ->
      do bracket (runTxSystem saver runRestoreEntryPoint) (shutdownSystem) $ \ctl ->
           do action ctl
         bracket (runTxSystem saver runRestoreEntryPoint) (shutdownSystem) $ \ctl ->
           do ls <- query Peek
              return (check ls)

-- State changes without a checkpoint should be lost.
prop_runRestoreId withSaver values
    = genRunRestoreProp withSaver (==values) $ \ctl ->
      forM_ values (update . Push)

-- Checkpoints can be placed arbitrarily.
prop_runRestoreCheckpoint withSaver before after
    = genRunRestoreProp withSaver (== (before++after)) $ \ctl ->
      do forM_ before (update . Push)
         createCheckpoint ctl
         forM_ after (update . Push)

-- Multiple checkpoints can be placed arbitrarily.
prop_runRestoreMultipleCheckpoint withSaver allData
    = genRunRestoreProp withSaver (== concat allData) $ \ctl ->
      do forM_ allData $ \dataList -> do createCheckpoint ctl
                                         forM_ dataList (update . Push)

-- 'update'/'update' and 'checkCheckpoint' are thread-safe.
prop_runRestoreAsync withSaver values
    = genRunRestoreProp withSaver (== values) $ \ctl ->
      do mv1 <- newEmptyMVar
         mv2 <- newEmptyMVar
         forkIO $ forM_ values (update . Push) `finally` putMVar mv1 ()
         forkIO $ createCheckpoint ctl `finally` putMVar mv2 ()
         takeMVar mv1
         takeMVar mv2

-- Send all events in parallel. Better at testing congestion.
prop_runRestoreCongestion withSaver values
    = genRunRestoreProp withSaver (\ls -> sum ls == sum values) $ \ctl ->
      do sem <- newQSem 0
         mv <- newEmptyMVar
         forM_ values $ \value -> forkIO (update (Push value) `finally` signalQSem sem)
         forkIO $ createCheckpoint ctl `finally` putMVar mv ()
         takeMVar mv
         replicateM_ (length values) (waitQSem sem)

checkCheckpointProperties :: IO ()
checkCheckpointProperties
    = tryTests ("runRestore") options [run (prop_runRestoreId saver)
                                      ,run (prop_runRestoreCheckpoint saver)
                                      ,run (prop_runRestoreMultipleCheckpoint saver)
                                      ,run (prop_runRestoreAsync saver)
                                      ,run (prop_runRestoreCongestion saver)]
    where options = defOpt{length_of_tests=5}
          saver = withQueueSaver withMemorySaver

