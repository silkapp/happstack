module HAppS.State.TxControl
    ( runTxSystem
    , runTxSystem'
    , shutdownSystem
    ) where

import System.Log.Logger
import System.IO
import Control.Monad
import Control.Exception
import Control.Concurrent

import HAppS.State.Checkpoint
import HAppS.State.Saver
import HAppS.State.Transaction
import HAppS.State.Spread
import HAppS.State.ComponentSystem
import HAppS.Data.Proxy

logMM = logM "HAppS.State.TxControl"


-- | Run a transaction system 
runTxSystem :: (Methods st, Component st) => Saver -> Proxy st -> IO (MVar TxControl)
runTxSystem = runTxSystem' False

runTxSystem' :: (Methods st, Component st) => Bool -> Saver -> Proxy st -> IO (MVar TxControl)
runTxSystem' withMultimaster saver stateProxy =
    do logMM NOTICE "Initializing system control."
       ctl <- createTxControl saver stateProxy
       logMM NOTICE "Creating event mapper."
       localEventMap <- createEventMap ctl stateProxy
       setNewEventMap localEventMap
       logMM NOTICE "Restoring state."
       enableLogging <- restoreState ctl
       when (withMultimaster)
            $ do logMM NOTICE "Multimaster mode"
                 cluster <- connectToCluster
                 eventMap <- changeEventMapping ctl localEventMap cluster
                 setNewEventMap eventMap
       enableLogging
       let ioActions = componentIO stateProxy
       logMM NOTICE "Forking children."
       children <- forM ioActions $ \action -> do mv <- newEmptyMVar
                                                  tid <- forkIO (action `finally` putMVar mv ())
                                                  return (tid,mv)
       modifyMVar_ ctl $ \ctl -> return ctl{ctlChildren = children}
       return ctl

shutdownSystem :: MVar TxControl -> IO ()
shutdownSystem ctl
    = do logMM NOTICE "Shutting down."
         children <- liftM ctlChildren $ readMVar ctl
         logMM NOTICE "Killing children."
         mapM_ (killThread . fst) children
         mapM_ (takeMVar . snd) children -- FIXME: Use a timeout.
         logMM NOTICE "Shutdown complete"
         closeTxControl ctl
