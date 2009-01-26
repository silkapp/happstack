{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module HAppS.State.Spread
    ( connectToCluster
    , changeEventMapping
    ) where

import HAppS.State.Transaction
import HAppS.Data.Serialize
import HAppS.Data.SerializeTH

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as Map

import Data.Typeable
import Spread.Client
import qualified Control.Concurrent.Chan.Closeable as Closeable

import System.Log.Logger
import Control.Monad hiding (join)
import Control.Concurrent
import System.Random

logSP = logM "HAppS.State.Spread"


data Cluster = Cluster
    { clusterChan       :: Closeable.Chan Closeable.R Message
    , clusterConnection :: Connection
    }

type EventId = Int

data ClusterMsg
    = RequestState
    | NewState B.ByteString -- A piece of state (map of all components)
    | StateTransferred      -- The entire state has been transmitted.
    | ForeignEvent EventId Object
    | EventResponse EventId Object
      deriving (Typeable)
instance Version ClusterMsg -- version 0
$(deriveSerialize ''ClusterMsg)

connectToCluster :: IO Cluster
connectToCluster
    = do logSP NOTICE "Connecting to spread daemon on localhost"
         uniq <- randomRIO (0,30000) -- FIXME: this is a hack to get unique names.
         let name = mkPrivateName (B.pack $ "node" ++ show (uniq::Int))
         (chan, conn) <- connect defaultConf{desiredName = name}
         startReceive conn
         join receiverGroup conn
         return $ Cluster chan conn

receiverGroup = let Just g = makeGroup "receiver"
                in g

changeEventMapping :: MVar TxControl -> EventMap -> Cluster -> IO EventMap
changeEventMapping ctlVar localEventMap cluster
    = do logSP NOTICE "Create new event mapper"
         members <- getClusterMembers cluster
         responseIndex <- newMVar Map.empty
         ready <- newEmptyMVar
         eventQueue <- newChan
         stateVar <- newMVar L.empty
         eidStore <- newMVar 0 -- eid's are locally unique.
         let newEID = modifyMVar eidStore (\store -> return (store+1,store+1))
             pushStateBlock st = modifyMVar_ stateVar (\acc -> return $ acc `L.append` L.fromChunks [st])
             insertEID eid = do mv <- newEmptyMVar
                                modifyMVar_ responseIndex $ \idx -> return $ Map.insert eid mv idx
                                return $ takeMVar mv
             returnResponse eid object = modifyMVar_ responseIndex $ \idx ->
                                         case Map.lookup eid idx of
                                           Nothing -> return idx -- We're already received the result from another node.
                                           Just mv -> do putMVar mv object -- Notify the caller about the new response.
                                                         return $ Map.delete eid idx
             listener = forever $
                        do mbMsg <- Closeable.readChan (clusterChan cluster)
                           case mbMsg of
                             Nothing -> error "Disconnected from the spread daemon."
                             Just (Regular msg) ->
                                 case fst (deserialize (L.fromChunks [inData msg])) of
                                   RequestState            -> sendState ctlVar (inSender msg) cluster
                                   ForeignEvent eid object -> writeChan eventQueue (inSender msg, eid, object)
                                   NewState st             -> pushStateBlock st
                                   -- FIXME: check that stateVar is non-empty?
                                   StateTransferred        -> do restoreState =<< takeMVar stateVar
                                                                 putMVar ready ()
                                   EventResponse eid object-> returnResponse eid object
                             Just _  -> return ()
             responder = forever $
                         do (sender, eid, object) <- readChan eventQueue
                            response <- runObjectEventFunc object localEventMap
                            sendClusterMsg [sender] (EventResponse eid response) cluster
         forkIO listener
         forkIO responder
         case members of
           [] -> putMVar ready () -- The cluster is empty, use state from disk.
           (x:_) -> do logSP NOTICE $ "Requesting state from: " ++ show x
                       sendClusterMsg [x] RequestState cluster
         logSP NOTICE "Waiting for ready signal"
         takeMVar ready
         logSP NOTICE "Ready signal received"
         let newEventMap = flip Map.map localEventMap $ \handler ->
                           case handler of
                             UpdateHandler runCold _ parse
                                 -> let runHot ev = do eid <- newEID
                                                       logSP NOTICE $ "New eid: " ++ show eid
                                                       wait <- insertEID eid
                                                       sendClusterMsg [receiverGroup] (ForeignEvent eid (mkObject ev)) cluster
                                                       response <- wait
                                                       logSP NOTICE $ "Received response for: " ++ show eid
                                                       return $ parseObject response
                                    in UpdateHandler runCold runHot parse
                             QueryHandler{} -> handler
         return newEventMap

sendState ctlVar sender cluster
    = withMVar ctlVar $ \ctl ->
      do logSP NOTICE $ "Sending state to: " ++ show sender
         allStates <- mapM getState (ctlAllComponents ctl)
         let chunks = L.toChunks $ serialize $ zip (ctlAllComponents ctl) allStates
         forM_ chunks $ \chunk ->
             do sendClusterMsg [sender] (NewState chunk) cluster
         sendClusterMsg [sender] StateTransferred cluster
         logSP NOTICE $ "The state has been transmitted."

restoreState :: L.ByteString -> IO ()
restoreState stateObject
    = do logSP NOTICE $ "Loading components from network."
         forM_ state $ \(stateType, stateData) ->
             do setNewState stateType stateData
         logSP NOTICE $ "All components successfully loaded"
    where state = fst (deserialize stateObject)

sendClusterMsg :: [Group] -> ClusterMsg -> Cluster -> IO ()
sendClusterMsg to msg cluster
    = send outMsg (clusterConnection cluster)
    where outMsg = Outgoing { outOrdering = Safe
                            , outDiscard = False
                            , outData = B.concat (L.toChunks (serialize msg))
                            , outGroups = to
                            , outMsgType = 0 }

-- Cluster members not including self.
getClusterMembers :: Cluster -> IO [PrivateGroup]
getClusterMembers cluster
    = do mbMsg <- Closeable.readChan (clusterChan cluster)
         case mbMsg of
           Just (Membership membership)
               -> case membership of
                    Reg{members=m} -> return $ filter (/= privateGroup (clusterConnection cluster)) m
                    Transient{} -> error $ "Received a Transient message on connect."
                    SelfLeave{} -> error $ "Received a SelfLeave message on connect."
           Just msg -> error $ "Expected membership message from cluster. Received a " ++ showMsgType msg ++ " message."
           Nothing  -> error "Cluster channel unexpectedly closed."

showMsgType (Regular _) = "regular"
showMsgType (Membership _) = "membership"
showMsgType (Rejected _) = "rejected"

