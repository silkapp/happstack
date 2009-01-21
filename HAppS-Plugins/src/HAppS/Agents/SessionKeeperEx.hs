{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -cpp #-}
module HAppS.Agents.SessionKeeperEx
{-    (-- * Types
     SessionStateEx, SessionKey,
     -- * Sessions
     newSession, getSession, setSession, delSession,
     setSessionTimeOut, pretty, setNewSession
    ) -} where

{-
import qualified Control.Monad.State as S
import GHC.Exts
import HAppS.MACID
import HAppS.MACID.Types
import HAppS.DBMS.RSMap as M
import HAppS.Util.Cron
import HAppS.Util.Common
import HAppS.Util.EventHandler
import HAppS.Util.StdMain.StartState

type SessionKey   = Integer
type SMap a = M.Map SessionKey (a,EpochSeconds)
type EvSession a r = forall sel ev. Ev (SessionStateEx sel a) ev r

instance EventHandler dispatch st ses () => StartStateEx st (SessionStateEx dispatch ses) where
    startStateExM _ = return startState
    runPartEx       = fmap (:[]) . run

#ifndef __HADDOCK__
data SessionStateEx dispatch ses where
    S :: EventHandler dispatch st ses () => Ev st ses () -> SMap ses -> SessionStateEx dispatch ses
#else
data SessionStateEx dispatch ses
#endif


instance Show ses => Show (SessionStateEx dis ses) where
    show (S _ smap) = show smap

instance (Read ses, EventHandler dis st ses ()) => Read (SessionStateEx dis ses) where
    readsPrec k i = map w $ readsPrec k i
        where w (sm,rest) = (S (eventHandler p) sm, rest)
              p :: Proxy dis
              p = Proxy

u_smap :: (SMap a -> SMap a) -> SessionStateEx d a -> SessionStateEx d a
u_smap f (S evh sm) = S evh $ f sm

u_smapM f = S.modify $ u_smap f

-- | Initial empty state.
startState :: forall dispatch st ses. EventHandler dispatch st ses () => SessionStateEx dispatch ses
startState  = S (eventHandler p)  M.empty
    where p :: Proxy dispatch
          p = Proxy

-- | Create a new session with the given lifetime in seconds relative to current time and return the new SessionKey.
newSession :: Seconds -> a -> EvSession a SessionKey
newSession ep v = do rnd <- getRandom
                     ep0 <- getTime
                     u_smapM $ M.insert (abs rnd) (v,fromIntegral ep+ep0)
                     return $ abs rnd

-- | Low level function to set  the absolute timeout for a SessionKey and the associated value.
setNewSession :: SessionKey -> EpochSeconds -> a -> EvSession a ()
setNewSession sk ep v = u_smapM  $ M.insert sk (v,ep)

-- | Get session data corresponding to the SessionKey or fail.
getSession :: SessionKey -> EvSession a a
getSession sk = do S _ smap <- S.get
                   case M.lookup sk smap of
                     Just (x,_) -> return x
                     Nothing    -> fail "Invalid session."

-- | Update session data corresponding to the SessionKey.
setSession :: SessionKey -> a -> EvSession a ()
setSession sk v = u_smapM $ M.update (\(_,ep) -> Just (v,ep)) sk

-- | Set a new absolute timeout associated with the session.
setSessionTimeOut :: SessionKey -> EpochSeconds -> EvSession a ()
setSessionTimeOut sk ep = u_smapM $ M.update (\(v,_) -> Just (v,ep)) sk

-- | Delete the session referred by the SessionKey.
delSession :: SessionKey -> EvSession a ()
delSession sk = u_smapM $ M.delete sk


run :: forall dis st ses.
       EventHandler dis st ses ()
    => (st -> SessionStateEx dis ses)
    -> (SessionStateEx dis ses -> st -> st)
    -> Handler st
run i u = everyNthSecond 60 $ do
  ep <- getTime
  let f (_,time) = time > ep
  smap <- gets_smap i
  let (keep,del) = M.partition f smap
  if M.null del then return () else do
  smod_usmapM i u keep
  evh <- sevh i
  flip mapM_ (M.elems del) $ \(elem,_) -> localEvent elem evh

gets_smap :: (st -> SessionStateEx dis ses) -> Ev st any (SMap ses)
gets_smap i = do
  S _ smap <- S.gets i
  return smap

smod_usmapM :: (st -> SessionStateEx dis ses) -> (SessionStateEx dis ses -> st -> st) -> SMap ses -> Ev st any ()
smod_usmapM i u new = do
  S evh _ <- S.gets i
  S.modify $ u $ S evh new

-- GHC does not get this typing right. So we use a sledgehammer...
-- System Fc will help with this, but won't help for the current
-- releases.
sevh :: forall dis st ses any. EventHandler dis st ses () => (st -> SessionStateEx dis ses) -> Ev st any (Ev st ses ())
sevh i = fmap f $ S.gets i
    where f :: SessionStateEx dis ses -> Ev st ses ()
          f (S evh _) = unsafeCoerce# evh

-- | Show the whole session state.
pretty :: Show a => EvSession a String
pretty = do S _ sm <- S.get
            return $ show sm
-}
