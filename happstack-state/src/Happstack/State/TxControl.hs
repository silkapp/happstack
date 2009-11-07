{-# LANGUAGE CPP #-}
module Happstack.State.TxControl
    ( runTxSystem
    , runTxSystemAmazon
    , shutdownSystem
    , createCheckpoint
    ) where

import System.Log.Logger
import System.IO
import Control.Monad
import Control.Exception
import Control.Concurrent

import qualified Happstack.State.Checkpoint as Checkpoint
import Happstack.State.Saver
import Happstack.State.Transaction

#ifdef REPLICATION
import qualified Happstack.State.CentralLogServer as LogServer
#endif

import Happstack.State.ComponentSystem
import Happstack.Data.Proxy

logMM :: Priority -> String -> IO ()
logMM = logM "Happstack.State.TxControl"


-- | Given a Saver and a Proxy, createTxControl will 
-- initialize a TxControl.  This does not actually start the
-- state system.
createTxControl :: (Methods state, Component state) =>
                   Saver -> Proxy state -> IO (MVar TxControl)
createTxControl saver prox
    = do 

         -- The state hasn't been loaded yet. Ignore events.
         eventSaverVar   <- newMVar =<< createWriter NullSaver "events" 0
         -- obtain a prefix lock
         lock <- obtainLock saver
         newMVar $ TxControl
                       { ctlSaver             = saver
                       , ctlEventSaver        = eventSaverVar
                       , ctlAllComponents     = allStateTypes prox
                       , ctlComponentVersions = componentVersions prox
                       , ctlChildren          = []
                       , ctlPrefixLock        = lock
                       , ctlCreateCheckpoint  = return () }


-- | Saves the state and closes the serialization
closeTxControl :: MVar TxControl -> IO ()
closeTxControl ctlVar
    = do ctl <- takeMVar ctlVar
         writerClose =<< takeMVar (ctlEventSaver ctl)
         releaseLock (ctlPrefixLock ctl)



-- | Run the MACID system without multimaster support and with the given Saver.
runTxSystem :: (Methods st, Component st) => Saver -> Proxy st -> IO (MVar TxControl)
runTxSystem saver stateProxy =
    do logMM NOTICE "Initializing system control."
       ctl <- createTxControl saver stateProxy
       -- insert code to lock based on the saver
       logMM NOTICE "Creating event mapper."
       localEventMap <- createEventMap ctl stateProxy
       setNewEventMap localEventMap
       logMM NOTICE "Restoring state."
       enableLogging <- Checkpoint.restoreState ctl
       -- Multimaster support used to be here. --
       enableLogging
       let ioActions = componentIO stateProxy
       logMM NOTICE "Forking children."
       children <- forM ioActions $ \action -> do mv <- newEmptyMVar
                                                  tid <- forkIO (action `finally` putMVar mv ())
                                                  return (tid,mv)
       modifyMVar_ ctl $ \c -> return c{ ctlChildren = children
                                       , ctlCreateCheckpoint = Checkpoint.createCheckpoint ctl }
       return ctl

#ifdef REPLICATION
runTxSystemAmazon :: (Methods st, Component st) => LogServer.ApplicationName -> Proxy st -> IO (MVar TxControl)
runTxSystemAmazon appName stateProxy
    = do logMM NOTICE "Initializing system control"
         ctl <- createTxControl NullSaver stateProxy
         logMM NOTICE "Creating local event mapper."
         localEventMap <- createEventMap ctl stateProxy
         logMM NOTICE "Connecting to central log server."
         cluster <- LogServer.connectToCluster appName localEventMap
         logMM NOTICE "Modifying local event map."
         eventMap <- LogServer.changeEventMapping localEventMap cluster
         setNewEventMap eventMap
         let ioActions = componentIO stateProxy
         logMM NOTICE "Forking children."
         children <- forM ioActions $ \action -> do mv <- newEmptyMVar
                                                    tid <- forkIO (action `finally` putMVar mv ())
                                                    return (tid,mv)
         modifyMVar_ ctl $ \c -> return c{ ctlChildren = children
                                         , ctlCreateCheckpoint = LogServer.createCheckpoint ctl cluster }
         return ctl
#else
type ApplicationName = String -- Hm, this should actually be defined in CentralLogServer.hs

runTxSystemAmazon :: (Methods st, Component st) => ApplicationName -> Proxy st -> IO (MVar TxControl)
runTxSystemAmazon appName stateProxy
    = error "Happstack-state has been built without replication support."
#endif


createCheckpoint :: MVar TxControl -> IO ()
createCheckpoint
    = join . fmap ctlCreateCheckpoint . readMVar

-- | Shuts down a transaction system
shutdownSystem :: MVar TxControl -> IO ()
shutdownSystem ctl
    = do logMM NOTICE "Shutting down."
         children <- liftM ctlChildren $ readMVar ctl
         logMM NOTICE "Killing children."
         mapM_ (killThread . fst) children
         mapM_ (takeMVar . snd) children -- FIXME: Use a timeout.
         logMM NOTICE "Shutdown complete"
         closeTxControl ctl

