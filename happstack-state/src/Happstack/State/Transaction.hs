{-# LANGUAGE CPP, TemplateHaskell, MagicHash,
    ExistentialQuantification, DeriveDataTypeable,
    GADTs, MultiParamTypeClasses #-}
module Happstack.State.Transaction where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception.Extensible
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as L
import Data.IORef
import System.IO.Unsafe
import System.Random
import System.Time(getClockTime,ClockTime(TOD))
import System.Log.Logger

import Happstack.State.ComponentSystem
import Happstack.State.Monad
import Happstack.State.Saver
import Happstack.Data.Serialize
import Happstack.Data.SerializeTH
import Happstack.State.Types
import Happstack.Util.Common (Seconds)

import Data.Dynamic
import GHC.Base

import Prelude hiding (catch)

import qualified Data.Binary as Binary

type ExceptionT = SomeException

logMT :: Priority -> String -> IO ()
logMT = logM "Happstack.State.Transaction"

getTime :: (Integral epochTime) => AnyEv epochTime
getTime = sel (fromIntegral . txTime . evContext)

getEventClockTime :: AnyEv ClockTime
getEventClockTime = do milliSeconds <- sel (txTime . evContext)
                       return $ TOD (fromIntegral milliSeconds) 0

getEventId :: (Integral txId) => AnyEv txId
getEventId = sel (fromIntegral . txId . evContext)


instance Version TxContext -- Default to version 0

$(deriveSerialize ''TxContext)

-- Isomorphic to (), we're interested in a descriptive name.
data GetCheckpointState = GetCheckpointState deriving (Typeable)
instance Version GetCheckpointState
$(deriveSerialize ''GetCheckpointState)

newtype SetCheckpointState = SetCheckpointState L.ByteString deriving (Typeable)
instance Version SetCheckpointState
$(deriveSerialize ''SetCheckpointState)


instance Version StdGen
instance Serialize StdGen where
    getCopy = contain $ liftM read safeGet
    putCopy = contain . safePut . show

{- Durablity:
* Pending queue is TChan (TxContext, ev)
* Get events from the input sources in circular fashion
* Dump events on disk before adding to pending queue
* Checkpoints as follows:
  * check point event arrives from one of the input sources
  * write a new checkpoint file with:
    + list of pending transactions (all non-pending are out of system)
    + next txid
    + save state
    + rotare log files
  * resume transaction processing


-}


type TypeString = String

data EventHandler where
    UpdateHandler :: UpdateEvent ev res =>
                     (TxContext -> ev -> IO ()) ->
                     (ev -> IO res) ->
                     (Object -> ev) ->
                     EventHandler
    QueryHandler :: QueryEvent ev res =>
                    (ev -> IO res) ->
                    (Object -> ev) ->
                    EventHandler

type EventMap = M.Map TypeString EventHandler
data EmitInternal = EmitInternal EventMap

{-# NOINLINE emitRef #-}
emitRef :: IORef EmitInternal
emitRef = unsafePerformIO $ newIORef (error "Happstack not initiated")

-- Low level function for emitting events. Very unsafe, do not expose.
emitFunc :: (Serialize ev, Typeable res) =>
            EventMap -> TypeString -> ev -> IO res
emitFunc eventMap eventType ev
     = case M.lookup eventType eventMap of
         Nothing -> error $ "Emitted event to unknown component. Ev: " ++ eventType
         Just (UpdateHandler _cold hot _parse) -> unsafeCoerce# hot ev
         Just (QueryHandler fn _parse) -> unsafeCoerce# fn ev

-- Wrapper around the global emitter map. Very unsafe, do not expose.
-- This function is only safe through 'query' and 'update'.
emitEvent' :: (Serialize ev, Typeable res) => TypeString -> ev -> IO res
emitEvent' eventType ev
    = do internal <- readIORef emitRef
         case internal of
           EmitInternal eventMap -> emitFunc eventMap eventType ev

emitEvent :: (Serialize ev, Typeable res) => ev -> IO res
emitEvent ev = emitEvent' (show (typeOf ev)) ev

setNewEventMap :: EventMap -> IO ()
setNewEventMap eventMap
    = writeIORef emitRef $ EmitInternal eventMap

data EventItem = EventItem
    { eventContext :: TxContext
    , eventData    :: Dynamic }

{-# NOINLINE eventStreamRef #-}
eventStreamRef :: MVar (Chan EventItem)
eventStreamRef = unsafePerformIO $ newMVar $ error "eventStreamRef not initialized."


initEventStream :: IO ()
initEventStream = do c <- newChan
                     forkIO $ forever $ readChan c -- Immediately discard items from this channel.
                     swapMVar eventStreamRef c
                     return ()

pushEventItem :: Serialize ev => TxContext -> ev -> IO ()
pushEventItem context ev
    = do c <- readMVar eventStreamRef
         writeChan c $ EventItem context (toDyn ev)

getEventStream :: IO (IO EventItem)
getEventStream = do c <- dupChan =<< readMVar eventStreamRef
                    return (readChan c)

{-
  Events for different components can be executed in parallel.
-}
-- Casting the event and the result type is safe. The types are kept sane due to the
-- EventUpdate and EventQuery classes.
createEventMap :: (Methods st, Component st) => MVar TxControl -> Proxy st -> IO EventMap
createEventMap ctlVar componentProxy
    = do initEventStream
         maps <- forM (M.elems componentTree) $ \(MethodMap m) ->
                 do tx <- createNewTxRun
                    ctl <- readMVar ctlVar
                    runTxLoop (ctlEventSaver ctl) (txProcessQueue tx) initialValue
                    return $ M.union (extraEvents tx) (M.map (eventHandler tx) m)
         return $ M.unions maps
    where (componentTree, _versions, _ioActions) = collectHandlers componentProxy
          eventHandler tx (Update fn)
              = let updateCold' cxt ev
                        = do mv <- newEmptyMVar
                             let handler = do lastCxt <- readTVar (txLastTxContext tx)
                                              if txId lastCxt < txId cxt
                                                 then do writeTVar (txLastTxContext tx) cxt
                                                         writeTChan (txProcessQueue tx) $ IHR cxt ev $
                                                           handleUpdate (putMVar mv) $ fn ev
                                                         return $ return ()
                                                 else return $ putMVar mv $ Right $ error "Cold event not executed"
                             join $ atomically $ handler
                             me <- takeMVar mv
                             case me of
                                 Left e -> throwIO e
                                 Right e -> return e
                    updateCold cxt ev = do updateCold' cxt ev; return ()
                    updateHot ev
                        = do cxt <- atomically . addTxId tx =<< newTxContext
                             updateCold' cxt ev
                in UpdateHandler updateCold updateHot parseObject
          eventHandler tx (Query fn)
              = let queryEmitter ev
                        = do mv <- newEmptyMVar
                             quickQuery' tx $ HR ev $ handleQuery (putMVar mv) (fn ev)
                             me <- takeMVar mv
                             case me of
                                 Left e -> throwIO e
                                 Right e -> return e
                in QueryHandler queryEmitter parseObject


instance QueryEvent GetCheckpointState L.ByteString
instance UpdateEvent SetCheckpointState ()

extraEvents :: Serialize st => TxRun st -> EventMap
extraEvents tx
    = M.fromList [ (getStateType stateType, getStateHandler tx)
                 , (setNewStateType stateType, setNewStateHandler tx)
                 ]
    where t :: TxRun st -> st
          t _ = undefined
          stateType = show (typeOf (t tx))
          getStateHandler tx'
              = let fn :: GetCheckpointState -> IO L.ByteString
                    fn ev = do mv <- newEmptyMVar
                               quickQuery' tx' $ HR ev $ \context st ->
                                 return (Nothing, putMVar mv (serialize (context, st)))
                               takeMVar mv
                in QueryHandler fn parseObject
          setNewStateHandler tx'
              = let fn :: SetCheckpointState -> IO ()
                    fn (SetCheckpointState bs)
                        =    do ((context, newState), rest) <- evaluate $ deserialize bs
                                unless (L.null rest) $ error $ "Junk after checkpoint for state: " ++ stateType
                                mv <- newEmptyMVar
                                quickQuery' tx' $ HR () $ \_context _oldState ->
                                    return (Just newState, putMVar mv ())
                                takeMVar mv
                                atomically $ writeTVar (txLastTxContext tx') context
                in UpdateHandler (error "No cold setState handler") fn parseObject


allStateTypes :: (Methods a, Component a) => Proxy a -> [TypeString]
allStateTypes prox = let (componentTree, _versions, _ioActions) = collectHandlers prox
                     in M.keys componentTree

componentVersions :: (Methods a, Component a) => Proxy a -> M.Map String [L.ByteString]
componentVersions prox = let (_componentTree, versions, _ioActions) = collectHandlers prox
                         in versions

componentIO :: (Methods a, Component a) => Proxy a -> [IO ()]
componentIO prox = let (_componentTree, _versions, ioActions) = collectHandlers prox
                   in ioActions

createNewTxRun :: IO (TxRun st)
createNewTxRun =
    atomically $
    do processQueue <- newTChan
       lastContext <- newTVar (TxContext 0 0 0 (mkStdGen 42))
       return $ TxRun processQueue lastContext

setNewStateType :: String -> String
setNewStateType str = "SetNewState: " ++ str

getStateType :: String -> String
getStateType str = "GetState: " ++ str

setNewState :: TypeString -> L.ByteString -> IO ()
setNewState stateType state
    = emitEvent' (setNewStateType stateType) (SetCheckpointState state)

getState :: TypeString -> IO L.ByteString
getState stateType
    = emitEvent' (getStateType stateType) GetCheckpointState

data SetNewState st = SetNewState L.ByteString deriving (Typeable)
data GetState st = GetState deriving (Typeable)

instance Version (SetNewState st)
instance Typeable st => Serialize (SetNewState st) where
    putCopy (SetNewState lbs) = contain $ Binary.put lbs
    getCopy = contain $ liftM SetNewState Binary.get
instance Version (GetState st)
instance Typeable st => Serialize (GetState st) where
    putCopy GetState = contain $ return ()
    getCopy = contain $ return GetState

instance Typeable st => UpdateEvent (SetNewState st) ()

instance Typeable st => QueryEvent (GetState st) L.ByteString





-- | Schedule an update and wait for it to complete. When this function returns, you're
-- guaranteed the update will be persistent.
update :: (MonadIO m, UpdateEvent ev res) => ev -> m res
update = liftIO . emitEvent

-- | Emit a state query and wait for the result.
query :: (MonadIO m, QueryEvent ev res) => ev -> m res
query = liftIO . emitEvent


-- Execute a query immediately without giving it a unique timestamp & transaction ID.
quickQuery' :: (Serialize st) => TxRun st -> HR st -> IO ()
quickQuery' txrun (HR ev fun)
    = do now <- getEpochMilli
         atomically $
           do tx <- readTVar (txLastTxContext txrun)
              writeTChan (txProcessQueue txrun) $ IHR tx{txTime=now} ev fun


type Runner ev res = IO (IO ev, res -> IO ())
type EH i o = i -> IO o

data Event = forall ev. Serialize ev => Event ev

data IHR st = forall ev. (Serialize ev)
    => IHR TxContext
           ev
           (RunHandler st ev)
data HR st = forall ev. (Serialize ev)
    => HR ev
          (RunHandler st ev)
type RunHandler st ev = TxContext -> st -> IO (Maybe st, IO ())
data Res a = Ok a | Error ExceptionT
type EventQueue st = TChan (HR st) -- Queue of local event not yet given a TxContext.
type ProcessQueue st = TChan (IHR st) -- Queue of events to be processed. TxContext'es have been asigned at this point.
data TxRun st   = TxRun {txProcessQueue  :: !(ProcessQueue st)
                        ,txLastTxContext :: !(TVar TxContext)}


type EvLoaders' st = M.Map String (ProcessQueue st -> L.ByteString -> IO (TxId,L.ByteString))
type EvLoaders =  M.Map String (L.ByteString -> IO (TxId,L.ByteString))

setEvLoadersQueue :: ProcessQueue st -> EvLoaders' st -> EvLoaders
setEvLoadersQueue queue = M.map (\fn -> fn queue)

-- serialized object -> serialized response
runObjectEvent :: Object -> IO Object
runObjectEvent obj
    = do EmitInternal eventMap <- readIORef emitRef
         runObjectEventFunc obj eventMap

runObjectEventFunc :: Object -> EventMap -> IO Object
runObjectEventFunc obj eventMap
    = do handler <- lookupEventHandler (objectType obj) eventMap
         case handler of
           -- FIXME: This will have to be filled in when doing sharding.
           QueryHandler{} -> error $ "Cold event was a query: " ++ objectType obj
           UpdateHandler _runCold runHot parse
               -> do res <- runHot (parse obj)
                     return $ mkObject res

runColdEvent :: TxContext -> Object -> IO ()
runColdEvent cxt obj
    = do EmitInternal eventMap <- readIORef emitRef
         runColdEventFunc cxt obj eventMap

runColdEventFunc :: TxContext -> Object -> EventMap -> IO ()
runColdEventFunc cxt obj eventMap
    = do handler <- lookupEventHandler (objectType obj) eventMap
         case handler of
           QueryHandler{} -> error $ "Cold event was a query: " ++ objectType obj
           UpdateHandler runCold _runHotObj parse
               -> do runCold cxt (parse obj)
                     return ()

lookupEventHandler :: TypeString -> EventMap -> IO EventHandler
lookupEventHandler eventType eventMap
    = case M.lookup eventType eventMap of
        Nothing -> error $ "Couldn't find handler for event of type: " ++ eventType
        Just handler -> return handler
        

eventTString :: Serialize ev => ev -> TypeString
eventTString ev = show (typeOf ev)



handleEvent :: (st -> Env -> Ev m res -> STM intermediate) -> (st -> intermediate -> IO (Maybe st, res))
            -> (Either ExceptionT res -> IO ()) -> Ev m res -> RunHandler st ev
handleEvent runner stateCheck ofun action tx st
    = handle eh $
      do intermediate <- atomically $ runQuery'
         (newState, res) <- stateCheck st intermediate
         return (newState, ofun (Right res))
    where runQuery' = do rs <- newTVar (txStdGen tx)
                         let env = Env { evContext = tx, evRandoms = rs }
                         intermediate <- runner st env action
                         return $ intermediate
          eh e = do logMT ERROR ("handleEvent FAIL: "++ show e)
                    return (Nothing,ofun (Left e))

handleQuery :: (Either ExceptionT res -> IO ()) -> Query st res -> RunHandler st ev
handleQuery = handleEvent (\st env (Ev cmd) -> runReaderT (cmd env) st) (\_st res -> return (Nothing, res))

handleUpdate :: (Either ExceptionT res -> IO ()) -> Update st res -> RunHandler st ev
handleUpdate = handleEvent (\st env (Ev cmd) -> runStateT (cmd env) st) (\st (res,st') -> checkDiff st st' >>= \diff -> return (diff, res))

{- Some updates might not modify the state.
   Doing a pointer-check might be worth it.
   (as a side note, reallyUnsafePtrEquality# is orders of magnitude faster
    than comparing StableNames.)
-}
checkDiff :: a -> a -> IO (Maybe a)
checkDiff _old new
    = return (Just new)

getEpochMilli :: IO EpochMilli
getEpochMilli =
    do TOD sec pico <- getClockTime
       return $ fromIntegral $ sec * 1000 + pico `div` 10^9

newTxContext :: IO TxContext
newTxContext = do
  milli <- getEpochMilli
  let txid = -1 -- Not set yet.
  sgen <- modifyMVar globalRandomGen (return . split)
  let (rand, sgen') = random sgen
  return $ TxContext txid rand milli sgen'

addTxId :: TxRun st -> TxContext -> STM TxContext
addTxId tx context
    = do lastContext <- readTVar (txLastTxContext tx)
         let new = context{txId = txId lastContext + 1}
         writeTVar (txLastTxContext tx) new
         return new{txId = txId new + 1}

{-# NOINLINE globalRandomGen #-}
-- XXX: why are we using a global StdGen? Isn't there already one in System.Random?
globalRandomGen :: MVar StdGen
globalRandomGen = unsafePerformIO (newMVar =<< getStdGen)


data TxConfig = TxConfig
    { txcCheckpointSeconds   :: Seconds,   -- ^ Perform checkpoint at least every N seconds.
      txcOperationMode       :: OperationMode,
      txcClusterSize         :: Int,       -- ^ Number of active nodes in the cluster (not counting this node).
      txcClusterPort         :: Int,       --
      txcCommitFrequency     :: Int        -- ^ Commits per second. Only applies to cluster mode.
    }

data TxControl = TxControl
    { ctlSaver      :: Saver           -- ^ Saver given by the user.
    , ctlEventSaver :: MVar (WriterStream EventLogEntry)
    , ctlAllComponents   :: [String]   -- ^ Types of each component used.
    , ctlComponentVersions :: M.Map String [L.ByteString] -- ^ Map listing all versions of a component
    , ctlChildren   :: [(ThreadId, MVar ())] -- 
    }

data EventLogEntry = EventLogEntry TxContext Object deriving (Typeable, Show)
instance Version EventLogEntry
instance Serialize EventLogEntry where
    putCopy (EventLogEntry context obj) = contain $ safePut (context,obj)
    getCopy = contain $ 
              do (context, obj) <- safeGet
                 return $ EventLogEntry context obj

data OperationMode
    = SingleMode
    | ClusterMode String

nullTxConfig :: TxConfig
nullTxConfig = TxConfig { txcCheckpointSeconds   = 60*60*24,
                          txcOperationMode       = SingleMode,
                          txcClusterSize         = 0,
                          txcClusterPort         = 8500,
                          txcCommitFrequency     = 50
                        }

runTxLoop :: MVar (WriterStream EventLogEntry) -> ProcessQueue st -> st -> IO ()
runTxLoop eventSaverVar queue st0 =
  let loop st = do
      IHR context ev fun <- atomically $ readTChan queue
      pushEventItem context ev -- Notify the user that this event is about to be executed.
                               -- FIXME: What if the user wants to know when the event has
                               -- finished executing?
      let tstring = eventTString ev
      logMT NOTICE $ ("> Event "++show (txId context)++" of "++tstring)
      (mst,ra) <- fun context st
      case mst of
            -- State was not updated.
            --
            -- Thus the response can be executed immediately.
            Nothing  -> do forkIO $ logMT NOTICE "> pure" >> ra
                           loop st 
            -- There is a new State.
            --
            -- Note that saverAdd can return without yet writing the result
            -- as long as:
            -- 1) saverAdd calls honor the sequence in which they were made.
            -- 2) saverAdd calls execute the finalizers only after the value
            --    has been serialized. The finalizers typically return the
            --    result to the user so they should not be kept
            --    waiting too long.
            -- 3) This means that checkpoints need to flush the saver
            --    which will guarantee that all pending result/side-effects
            --    have been processed.
            -- 4) Savers must *not* block while running the finalizers
            Just st' -> do eventSaver <- readMVar eventSaverVar
                           writerAdd eventSaver (EventLogEntry context (mkObject ev)) (logMT NOTICE "> disk " >> ra)
                           loop st'
  in do forkIO $ (loop st0) `catch` (\ThreadKilled -> return ())
                            `catch` (\BlockedIndefinitely -> return ())
        return ()

