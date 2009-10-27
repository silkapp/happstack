module Happstack.State.CentralLogServer where

import Data.Binary
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Concurrent
import System.Process
import System.Exit
import System.IO
import Control.Applicative ((<$>), (<*>))
import Data.Int
import Data.List
import Text.Printf
import Control.Exception
import qualified Data.Map as Map
import Control.Monad
import System.Random (mkStdGen)
import Network.AWS.AWSConnection
import Network.AWS.Authentication
import Network.AWS.S3Bucket
import Network.AWS.S3Object

import Happstack.Data.Serialize
import Happstack.State.Transaction
import Happstack.State.Types
import Happstack.State.NetworkChan
import Data.BEncode
import Data.BEncode.Parser

import Data.Digest.Pure.SHA

import System.Log.Logger

logLS :: Priority -> String -> IO ()
logLS = logM "Happstack.State.CentralLogServer"

type ApplicationName = String

data Entry = Entry EntryId EpochMilli EntryData deriving (Show)
type EntryId = Int64
type EntryData = ByteString
type URL = String

instance Binary Entry where
    get = Entry <$> get <*> get <*> get
    put (Entry eid epoch edata)
        = put eid >> put epoch >> put edata

data ForeignEvent = ForeignEvent L.ByteString L.ByteString
instance Binary ForeignEvent where
    get = ForeignEvent <$> get <*> get
    put (ForeignEvent hash event)
        = put hash >> put event

data CheckpointMsg = NoCheckpoint | Checkpoint EntryId URL deriving (Show)
instance BEncodeable CheckpointMsg where
    bencode NoCheckpoint = BString (L.pack "NoCheckpoint")
    bencode (Checkpoint eid url) = BDict (Map.singleton "Checkpoint" $ BDict $ Map.fromList [ ("entry_id", BInt $ fromIntegral eid)
                                                                                            , ("url", BString (encode url))])
    bdecode = do str <- bstring token
                 guard (str == "NoCheckpoint")
                 return NoCheckpoint
              <|>
              do setInput =<< dict "Checkpoint"
                 eid <- bint (dict "entry_id")
                 url <- bbytestring (dict "url")
                 return (Checkpoint (fromIntegral eid) $ decode url)

instance BEncodeable Entry where
    bencode entry = BString (encode entry)
    bdecode = fmap decode (bbytestring token)

instance BEncodeable L.ByteString where
    bencode bs = BString bs
    bdecode = bbytestring token

data UserMsg = NewEvent L.ByteString | NewCheckpoint EntryId URL deriving (Show)
instance BEncodeable UserMsg where
    bencode (NewEvent event)
        = BDict (Map.singleton "NewEvent" (BString event))
    bencode (NewCheckpoint eid url)
        = BDict $ Map.singleton "NewCheckpoint" $ BDict $ Map.fromList [ ("entry_id", BInt $ fromIntegral eid)
                                                                       , ("url", BString $ encode url) ]
    bdecode = do setInput =<< dict "NewEvent"
                 fmap NewEvent (bbytestring token)
              <|>
              do setInput =<< dict "NewCheckpoint"
                 eid <- bint (dict "entry_id")
                 url <- bbytestring (dict "url")
                 return (NewCheckpoint (fromIntegral eid) (decode url))

data Cluster
    = Cluster { clusterChan :: NetworkChan
              , clusterS3   :: AWSConnection
              , clusterBucket :: String
              , clusterLastId :: MVar EntryId }

logServerAddress = "174.129.13.114"

-- Connect to the central log server via SSH.
-- The appropriate identities have to be added to the autentication agent. See ssh-add(1).
connectToCluster :: ApplicationName -> EventMap -> IO Cluster
connectToCluster appName localEventMap
    = do logLS NOTICE "Retrieving S3 credentials from environment."
         Just conn <- amazonS3ConnectionFromEnv
         bucket <- initiateBucket conn -- FIXME: Run this function in 'createCheckpoint' instead?
         logLS NOTICE "Running LogRelay through ssh."
         (inh, outh) <- sshConnect ("app_" ++ appName) logServerAddress "LogRelay"
         logLS NOTICE "Creating network channel."
         chan <- newNetworkChan inh outh
         checkpoint <- peek chan :: IO CheckpointMsg
         lastId <- newEmptyMVar
         case checkpoint of
           NoCheckpoint       -> do putMVar lastId 0
                                    logLS NOTICE "No checkpoint available."
           Checkpoint eid url -> do putMVar lastId eid
                                    let (bucket,key) = read url
                                    logLS NOTICE $ "Checkpoint available from this location: " ++ show (bucket,key)
                                    ret <- getObject conn (S3Object { obj_bucket = bucket
                                                                    , obj_name   = key
                                                                    , content_type = ""
                                                                    , obj_headers = []
                                                                    , obj_data    = L.empty })
                                    case ret of
                                      Left err  -> error (show err)
                                      Right obj -> do (stateMap, _rest) <- evaluate $ deserialize (obj_data obj)
                                                      forM_ (Map.toList stateMap) $ \(stateTy, state) ->
                                                        setNewState' localEventMap stateTy state
         return Cluster { clusterChan = chan
                        , clusterS3   = conn
                        , clusterBucket = bucket
                        , clusterLastId = lastId }

createCheckpoint :: MVar TxControl -> Cluster -> IO ()
createCheckpoint ctlVar cluster
    = withMVar ctlVar $ \ctl ->
      do lastId <- readMVar (clusterLastId cluster)
         allStates <- mapM getState (ctlAllComponents ctl)
         let stateMap = Map.fromList (zip (ctlAllComponents ctl) allStates)
             checkpoint = serialize stateMap
         let bucket = clusterBucket cluster
             key    = "checkpoint_" ++ show (sha1 checkpoint)
         sendObject (clusterS3 cluster) (S3Object { obj_bucket   = bucket
                                                  , obj_name     = key
                                                  , content_type = "application/octet-stream"
                                                  , obj_headers  = []
                                                  , obj_data     = checkpoint })
         let url = show (bucket, key)
         poke (clusterChan cluster) (NewCheckpoint lastId url)

{- Find a bucket to place new checkpoints in. If no suitable bucket exists, create one.
   Race conditions aren't important since we're just trying to keep the happs buckets to a 
   minimum (preferable just one).
-}
initiateBucket conn
    = do buckets <- listBuckets conn
         case buckets of
           Left err      -> error (show err)
           Right buckets -> case [ bucket_name bucket | bucket <- buckets, "happs_checkpoints" `isPrefixOf` bucket_name bucket ] of
                              (bucket:_) -> return bucket
                              [] -> do ret <- createBucketWithPrefix conn "happs_checkpoints"
                                       case ret of
                                         Left err -> error (show err)
                                         Right bucket -> return bucket


changeEventMapping :: MVar TxControl -> EventMap -> Cluster -> IO EventMap
changeEventMapping ctlVar localEventMap cluster
    = do logLS NOTICE "Create new event mapper"
         responseIndex <- newMVar Map.empty
         let chan = clusterChan cluster
         let insertEID hash = do mv <- newEmptyMVar
                                 modifyMVar_ responseIndex $ \idx -> return $ Map.insert hash mv idx
                                 return $ takeMVar mv
             returnResponse hash object = modifyMVar_ responseIndex $ \idx ->
                                          case Map.lookup hash idx of
                                            Nothing -> return idx -- We're already received the result from another node.
                                            Just mv -> do putMVar mv object -- Notify the caller about the new response.
                                                          return $ Map.delete hash idx
             listener = forever $
                        do Entry eid time foreign <- peek chan
                           let ForeignEvent hash object = decode foreign
                           logLS NOTICE $ "Received entry with hash: " ++ show hash
                           let txContext = TxContext { txId = eid
                                                     , txRand = 0
                                                     , txTime = time
                                                     , txStdGen = mkStdGen 0 }
                           response <- runColdEventFunc txContext (fst $ deserialize object) localEventMap
                           swapMVar (clusterLastId cluster) eid
                           returnResponse hash response
         forkIO listener
         let newEventMap = flip Map.map localEventMap $ \handler ->
                           case handler of
                             UpdateHandler runCold _ parse
                                 -> let runHot ev = do let obj = serialize $ mkObject ev
                                                           hash = bytestringDigest $ sha1 obj
                                                           foreign = ForeignEvent hash obj
                                                       logLS NOTICE $ "New hash: " ++ show hash
                                                       wait <- insertEID hash
                                                       poke chan (NewEvent (encode foreign))
                                                       response <- wait
                                                       logLS NOTICE $ "Received response for: " ++ show hash
                                                       return $ parseObject response
                                    in UpdateHandler runCold runHot parse
                             QueryHandler{} -> handler
         return newEventMap



type UserName = String
type ServerAddress = String
type Command = String

sshConnect :: UserName -> ServerAddress -> Command -> IO (Handle, Handle)
sshConnect userName serverAddress command
    = do (inh, outh, errh, pid) <- runInteractiveProcess "ssh" ["-o", "BatchMode=yes", userName ++ "@" ++ serverAddress, command] Nothing Nothing
         tid <- myThreadId
         forkIO $ do errMsg <- hGetLine errh
                     hPutStrLn stderr $ "SSH tunneling failed: " ++ errMsg
                     throwTo tid (ExitFailure 255)
         return (inh, outh)
