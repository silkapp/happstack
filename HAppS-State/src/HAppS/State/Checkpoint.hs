{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE CPP, TemplateHaskell #-}
module HAppS.State.Checkpoint
    ( createTxControl
    , closeTxControl
    , restoreState
    , createCheckpoint
    ) where


import HAppS.State.Saver
import HAppS.Data.Serialize
import HAppS.Data.SerializeTH
import HAppS.State.Transaction
import HAppS.State.ComponentSystem

import Data.Typeable
import Data.Maybe
import Control.Concurrent
import Control.Monad
import Control.Exception as E
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M
import System.IO
import qualified Data.Map as M

import System.Log.Logger

logMC = logM "HAppS.State.Checkpoint"

{- State on disk:

* ${TXD}/events       event files in ascending order
* ${TXD}/checkpoints  checkpoint files in ascending order
* ${TXD}/current      pointer to last checkpoint

-}

data State = State
    { stateVersion :: Int
    , stateCutoff  :: Int
    } deriving (Typeable,Show)

instance Version State
#ifndef __HADDOCK__
$(deriveSerialize ''State)
#else
instance Serialize State
#endif

createTxControl :: (Methods state, Component state) =>
                   Saver -> Proxy state -> IO (MVar TxControl)
createTxControl saver proxy
    = do -- The state hasn't been loaded yet. Ignore events.
         eventSaverVar   <- newMVar =<< createWriter NullSaver "events" 0
         newMVar $ TxControl
                       { ctlSaver             = saver
                       , ctlEventSaver        = eventSaverVar
                       , ctlAllComponents     = allStateTypes proxy
                       , ctlComponentVersions = componentVersions proxy
                       , ctlChildren          = [] }

closeTxControl :: MVar TxControl -> IO ()
closeTxControl ctlVar
    = do ctl <- takeMVar ctlVar
         writerClose =<< takeMVar (ctlEventSaver ctl)

-- FIXME: It may be nice to print out what components were saved on disk
--        compared to the components actually used in the application.
-- | Load state from disk and re-run any needed events to
--   fully restore the state. The returned function enables
--   event logging.
restoreState :: MVar TxControl -> IO (IO ())
restoreState ctlVar
    = withMVar ctlVar $ \ctl ->
      do -- Find the last saved cutoff point.
         mbState <- readState ctl
         case mbState of
           Nothing ->
               do writeState ctl (State 0 0)
                  -- No events to replay. Switch to real saver.
                  return $ do swapMVar (ctlEventSaver ctl) =<< createWriter (ctlSaver ctl) "events" 0
                              return ()
           Just state ->
               do let cutoff = stateCutoff state
                  -- Load state and replay events.
                  loadState ctl cutoff
                  offset <- loadEvents ctl cutoff
                  -- We use a NullSaver when replaying events. Switch to real saver.
                  return $ do swapMVar (ctlEventSaver ctl) =<< createWriter (ctlSaver ctl) "events" (cutoff+offset)
                              return ()

-- Load state from disk.
loadState :: TxControl -> Int -> IO ()
loadState ctl cutoff
    = do logMC NOTICE $ "Loading components from storage."
         checkpoints     <- withReader ctl "checkpoints" cutoff $ loadCheckpoints
         forM_ (ctlAllComponents ctl) $ \stateType
             -> case M.lookup stateType (ctlComponentVersions ctl) of
                  Just versions -> forM_ versions $ \stateTypeVersion 
                      -> case M.lookup (L.unpack stateTypeVersion) checkpoints of
                           Just state -> setNewState stateType state
                           Nothing    -> return ()
                  Nothing       -> return ()
         -- FIXME: Prints stats about which components weren't found on disk
         --        and which components weren't found in the dependency tree.
         logMC NOTICE "All components successfully loaded"

-- Read and execute events since last checkpoint.
loadEvents :: TxControl -> Int -> IO Int
loadEvents ctl cutoff
    = do logMC NOTICE "Loading events from storage"
         (events, offset) <- withReader ctl "events" cutoff $ readerGet
         -- Execute events. Events that predate the last checkpoint aren't executed.
         forM_ events $ \(EventLogEntry context object) -> runColdEvent context object
         logMC NOTICE "All events successfully replayed."
         return offset

withReader ctl key cutoff
    = bracket (createReader (ctlSaver ctl) key cutoff)
              (readerClose)

withWriter ctl key cutoff
    = bracket (createWriter (ctlSaver ctl) key cutoff)
              (writerClose)

readState ctl
    = withReader ctl "current" 0 $ \s ->
      liftM listToMaybe $ readerGetUncut s

writeState :: TxControl -> State -> IO ()
writeState ctl state
    = bracket (createWriter (ctlSaver ctl) "current" 0)
              (writerClose)
              (\saver -> writerAtomicReplace saver state)

-- FIXME: Show process while loading state?
-- Might not be useful here since we just load the raw data.
-- The time consuming parsing takes place later on.
-- | Load a map from component types to serialized states.
loadCheckpoints :: ReaderStream (M.Map String L.ByteString) -> IO (M.Map String L.ByteString)
loadCheckpoints saver
    = do checkpointss <- readerGetUncut saver
         case checkpointss of
           [checkpoints] -> return checkpoints
           []            -> return M.empty
           _             -> error "Failed to read checkpoints."

saveCheckpoints :: WriterStream (M.Map String L.ByteString) -> M.Map String L.ByteString -> IO ()
saveCheckpoints saver checkpoints
    = do mv <- newEmptyMVar
         writerAdd saver checkpoints (putMVar mv ())
         takeMVar mv

-- Each checkpoint has a separate event queue. We don't want
-- to clear the queues since that would block event execution.
-- Hence, events may be written to the log after we make the cut
-- and before the state is saved. This means that some events
-- may need to be discarded next time the state is restored.
createCheckpoint :: MVar TxControl -> IO ()
createCheckpoint ctlVar
    = withMVar ctlVar $ \ctl ->
      do logMC NOTICE "Initiating checkpoint."
         newCut <- writerCut =<< readMVar (ctlEventSaver ctl)
         withWriter ctl "checkpoints" newCut $ \saver ->
             do allStates <- mapM getState (ctlAllComponents ctl)
                saveCheckpoints saver (M.fromList $ zip (ctlAllComponents ctl) allStates)
         writeState ctl $ State {stateVersion = 0
                                ,stateCutoff  = newCut}
         logMC NOTICE "Checkpoint successfully serialized."

