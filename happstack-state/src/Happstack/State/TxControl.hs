module Happstack.State.TxControl
    ( runTxSystem
    , runTxSystem'
    , shutdownSystem
    ) where

import System.Log.Logger
import System.IO
import Control.Monad
import Control.Exception
import Control.Concurrent

import Happstack.State.Checkpoint
import Happstack.State.Saver
import Happstack.State.Transaction
import Happstack.State.Spread
import Happstack.State.ComponentSystem
import Happstack.Data.Proxy

logMM :: Priority -> String -> IO ()
logMM = logM "Happstack.State.TxControl"

-- | Run the MACID system without multimaster support and with the given Saver.
runTxSystem :: (Methods st, Component st) => Saver -> Proxy st -> IO (MVar TxControl)
runTxSystem = runTxSystem' False

-- | Run the MACID system with multimaster support turned on if the first
-- argument is True.
runTxSystem' :: (Methods st, Component st) => Bool -> Saver -> Proxy st -> IO (MVar TxControl)
runTxSystem' withMultimaster saver stateProxy =
    do logMM NOTICE "Initializing system control."
       isLocked <- lock saver
       if isLocked 
         then aux
         else do putStrLn "There may already be an instance of this application running, which could result in a loss of data."
                 putStrLn "You need to manually remove the lock file in order to start the application."
                 error "Lock file error"
 where aux = do
         ctl <- createTxControl saver stateProxy
         -- insert code to lock based on the saver
         logMM NOTICE "Creating event mapper."
         localEventMap <- createEventMap ctl stateProxy
         setNewEventMap localEventMap
         logMM NOTICE "Restoring state."
         enableLogging <- restoreState ctl
         when withMultimaster
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
         modifyMVar_ ctl $ \c -> return c{ctlChildren = children}
         return ctl


-- | Shuts down a transaction system
shutdownSystem :: MVar TxControl -> IO ()
shutdownSystem ctl
    = do logMM NOTICE "Shutting down."
         saver <- liftM ctlSaver $ readMVar ctl
         children <- liftM ctlChildren $ readMVar ctl
         logMM NOTICE "Killing children."
         mapM_ (killThread . fst) children
         mapM_ (takeMVar . snd) children -- FIXME: Use a timeout.
         logMM NOTICE "Shutdown complete"
         closeTxControl ctl
         unlock saver
