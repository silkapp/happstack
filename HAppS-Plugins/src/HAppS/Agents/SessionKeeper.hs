{-# LANGUAGE TemplateHaskell, UndecidableInstances #-}
module HAppS.Agents.SessionKeeper
    (-- * Types
     Session, SessionKey,
     -- * Sessions
     newSession, setNewSession, getSession, delSession,
     setSession, setSessionTimeout, touchSession
    ) where


import HAppS.State
import HAppS.StdMain.StartState
import HAppS.Util.Common
import HAppS.Data

import Data.Typeable
import Control.Monad.State
import Control.Monad.Reader
import System.Random

import qualified Data.Map as M


$(deriveAll [''Read,''Show]
  [d|
      type SessionKey a = Integer
      newtype Session a = Session { unSession :: (M.Map (SessionKey a) (a,Seconds,EpochMilli)) }
   |]
 )


data NewSession a = NewSession (SessionKey a) Seconds a deriving (Typeable, Read, Show)
data GetSession a = GetSession (SessionKey a) deriving (Typeable, Read, Show)
data SetSession a = SetSession (SessionKey a) a deriving (Typeable, Read, Show)
data SetSessionTimeout a = SetSessionTimeout (SessionKey a) Seconds deriving (Typeable, Read, Show)
data DelSession a = DelSession (SessionKey a) deriving (Typeable, Read, Show)
data TouchSession a = TouchSession (SessionKey a) deriving (Typeable, Read, Show)

instance (Typeable a, Read a, Show a) => Serialize (NewSession a) where
    encodeBinary = defaultEncodeString
    decodeBinary = defaultDecodeString
instance (Typeable a, Read a, Show a) => Serialize (GetSession a) where
    encodeBinary = defaultEncodeString
    decodeBinary = defaultDecodeString
instance (Typeable a, Read a, Show a) => Serialize (SetSession a) where
    encodeBinary = defaultEncodeString
    decodeBinary = defaultDecodeString
instance (Typeable a, Read a, Show a) => Serialize (SetSessionTimeout a) where
    encodeBinary = defaultEncodeString
    decodeBinary = defaultDecodeString
instance (Typeable a, Read a, Show a) => Serialize (DelSession a) where
    encodeBinary = defaultEncodeString
    decodeBinary = defaultDecodeString
instance (Typeable a, Read a, Show a) => Serialize (TouchSession a) where
    encodeBinary = defaultEncodeString
    decodeBinary = defaultDecodeString







-- FIXME: IoH (fn hCleanUpSession)
--localState :: (outer -> inner) -> (inner -> outer -> outer) -> Ev (StateT inner STM) a -> Ev (StateT outer STM) a
instance (Typeable a, Read a, Show a) => StartStateEx outer (Session a) where
    startStateExM _ = return $ Session M.empty
    runPartEx get upd
        = let fn g x = localState get upd (g x)
          in [ UpdateH (fn hNewSession)
             , QueryH (\x -> localStateReader get (hGetSession x))
             , UpdateH (fn hSetSession)
             , UpdateH (fn hSetSessionTimeout)
             , UpdateH (fn hDelSession)
             , UpdateH (fn hTouchSession) ]



-- | Initiate a new session value and return the associated key.
newSession :: UpdateEvent (NewSession a) () => Seconds -> a -> IO (SessionKey a)
newSession ep v = do rnd <- randomIO
                     update (NewSession (abs rnd) ep v)
                     return rnd

-- | Initiate a new session value with a pre-generated key.
setNewSession :: (UpdateEvent (NewSession a) ()) => SessionKey a -> Seconds -> a -> IO ()
setNewSession key ep v = update (NewSession key ep v)

-- FIXME: Touch the value.
-- | Query the session state for a specific key.
getSession :: (QueryEvent (GetSession a) (Maybe a)) => SessionKey a -> IO (Maybe a)
getSession key = query (GetSession key)

-- | Replace the value associated with a specific key.
setSession :: (UpdateEvent (SetSession a) ()) => SessionKey a -> a -> IO ()
setSession key v = update (SetSession key v)

-- | Replace the timeout associated with a specific key. This touches the value.
setSessionTimeout :: forall a. (UpdateEvent (SetSessionTimeout a) ()) => SessionKey a -> Seconds -> IO ()
setSessionTimeout key t = update (SetSessionTimeout key t :: SetSessionTimeout a)

-- | Destroy the session value associated with the key.
delSession :: forall a. (UpdateEvent (DelSession a) ()) => SessionKey a -> IO ()
delSession key = update (DelSession key :: DelSession a)

-- | Update the session timestamp.
touchSession :: forall a. (UpdateEvent (TouchSession a) ()) => SessionKey a -> IO ()
touchSession key = update (TouchSession key :: TouchSession a)

hNewSession (NewSession sessionKey ep v)
    = do ep0 <- getTime
         modify $ Session . M.insert sessionKey (v,ep,fromIntegral ep+ep0) . unSession
         return ()

hGetSession :: GetSession a -> Query (Session a) (Maybe a)
hGetSession (GetSession sessionKey)
    = do now <- getTime
         asks $ \s -> case M.lookup sessionKey (unSession s) of
                        Just (v,_,t) | now < t -> Just v
                        _ -> Nothing

hSetSession (SetSession sessionKey v)
    = modify $ Session . M.adjust (\(_,s,t) -> (v,s,t)) sessionKey . unSession

hSetSessionTimeout :: forall a. SetSessionTimeout a -> Update (Session a) ()
hSetSessionTimeout (SetSessionTimeout sessionKey timeout)
    = do now <- getTime
         modify $ Session . M.adjust (\(v,_,_) -> (v,timeout,now+fromIntegral timeout*1000)) sessionKey . unSession
         return (error "setSessionTimeout dummy")

hDelSession :: DelSession a -> Update (Session a) ()
hDelSession (DelSession sessionKey)
    = do modify $ Session . M.delete sessionKey . unSession
         return (error "delSession dummy")

hTouchSession :: TouchSession a -> Update (Session a) ()
hTouchSession (TouchSession sessionKey)
    = do now <- getTime
         modify $ Session . M.adjust (\(v,s,_) -> (v,s,now+fromIntegral s*1000)) sessionKey . unSession

instance (StartStateEx outer (Session a), Typeable a, Read a, Show a) => UpdateEvent (NewSession a) ()
instance (StartStateEx outer (Session a), Typeable a, Read a, Show a) => QueryEvent (GetSession a) (Maybe a)
instance (StartStateEx outer (Session a), Typeable a, Read a, Show a) => UpdateEvent (SetSession a) ()
instance (StartStateEx outer (Session a), Typeable a, Read a, Show a) => UpdateEvent (SetSessionTimeout a) ()
instance (StartStateEx outer (Session a), Typeable a, Read a, Show a) => UpdateEvent (DelSession a) ()
instance (StartStateEx outer (Session a), Typeable a, Read a, Show a) => UpdateEvent (TouchSession a) ()



--instance StartState simple => StartStateEx outer simple where


{-
type SessionKey = String
type SessionMap a = Map SessionKey (a,EpochSeconds)


newSession :: Seconds -> a -> SessionEv a SessionKey
setNewSession :: SessionKey -> Seconds -> a -> SessionEv s ()
getSession :: SessionKey -> SessionEv a a
setSession :: SessionKey -> a -> SessionEv a ()
setSessionTimeOut :: SessionKey -> EpochSeconds -> SessionEv a ()


delSession :: SessionKey -> Ev st a

 delSessoin key :: Query MyUserData [String]


delSession "X"

with

class HasPassNet a where
    getPassNetState :: Ev st PassNetState
    setPassNetState :: PassNetState -> Ev st ()
-}


{-
import qualified Control.Monad.State as S
import Data.Typeable
import HAppS.MACID
import HAppS.DBMS.RSMap as M
import HAppS.Util.Cron
import HAppS.Util.Common
import HAppS.Util.StdMain.StartState


type SessionKey   = Integer
type SMap a = M.Map SessionKey (a,EpochSeconds)
type EvSession a r = Ev (SMap a) r

data Request = RunCleanup deriving(Read,Show,Typeable)
instance Serialize Request where
    typeString _    = "Happs.Agents.SessionKeeper.Request"
    encodeStringM _ = return ""
    decodeStringM s = return (RunCleanup, s)


instance StartState (State a) where
    startStateM = return startState
    runPart     = fmap (:[]) . sessionKeeper


data SessionState a = State { smap :: SMap a } deriving(Read,Show,Typeable)
type State a = SessionState a

-- | Initial empty state.
startState  = State M.empty

-- | Create a new session with the given lifetime in seconds relative to current time and return the new SessionKey.
newSession :: Seconds -> a -> EvSession a SessionKey
newSession ep v = do rnd <- getRandom
                     ep0 <- getTime
                     S.modify $ \st -> st { smap = M.insert (abs rnd) (v,fromIntegral ep+ep0) (smap st) }
                     return $ abs rnd

-- | Low level function to set  the absolute timeout for a SessionKey and the associated value.
setNewSession :: SessionKey -> EpochSeconds -> a -> EvSession a ()
setNewSession sk ep v = S.modify $ \st -> st {smap=M.insert sk (v,ep) $ smap st}

-- | Get session data corresponding to the SessionKey or fail.
getSession :: SessionKey -> EvSession a a
getSession sk = do st <- S.get
                   case M.lookup sk $ smap st of
                     Just (x,_) -> return x
                     Nothing    -> fail "Invalid session."

-- | Update session data corresponding to the SessionKey.
setSession :: SessionKey -> a -> EvSession a ()
setSession sk v = S.modify $ \st -> st { smap = M.update (\(_,ep) -> Just (v,ep)) sk $ smap st }

-- | Set a new absolute timeout associated with the session.
setSessionTimeOut :: SessionKey -> EpochSeconds -> EvSession a ()
setSessionTimeOut sk ep = S.modify $ \st -> st { smap = M.update (\(v,_) -> Just (v,ep)) sk $ smap st }

-- | Delete the session referred by the SessionKey.
delSession :: SessionKey -> EvSession a ()
delSession sk = S.modify $ \st -> st { smap = M.delete sk $ smap st }

sessionKeeper :: (st -> State a) -> (State a -> st -> st) -> Handler st
sessionKeeper ifun ufun = everyNthSecond 60 $ localState ifun ufun $ do
    ep <- getTime
    let f (_,time) = time > ep
    S.modify $ \st -> st { smap = M.filter f $ smap st }



-- | Show the whole session state.
pretty :: Show a => EvSession a String
pretty = fmap show S.get
-}
