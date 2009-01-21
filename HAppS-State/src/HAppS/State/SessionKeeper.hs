{-# LANGUAGE TemplateHaskell, UndecidableInstances, CPP #-}
{-# OPTIONS -fglasgow-exts #-}
module HAppS.State.SessionKeeper where


import HAppS.State
import HAppS.State.ComponentTH
import HAppS.State.ComponentTypes
import HAppS.Util.Common
import HAppS.Data

import Data.Typeable
import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Map as M

instance StartState (M.Map a b) where
    startState = return $ M.empty

#ifndef __HADDOCK__
$(deriveAll [''Read,''Show]
  [d|
      type SessionKey = Integer
      newtype Session a = Session { unSession :: (M.Map SessionKey (a,Seconds,EpochMilli)) }
      data UserComponent key = UserComponent key (Component (Session Int))
      data IxSet a = IxSet a
      type TySynComponent key1 key2 = IxSet (key1,key2)
   |]
 )
#endif

instance StartState a => StartState (IxSet a) where
    startState = liftM IxSet startState

methodOne :: (Ord key1, Eq key2) => key2 -> key1 -> Query (TySynComponent key1 key2) ()
methodOne _ _ = return ()

$(methods_ SerializeString ''TySynComponent ['methodOne])
$(atStart ''TySynComponent [])

{-
newSession :: SessionKey -> Seconds -> v -> Update (Session v) ()
newSession sessionKey timeout v
    = do now <- getTime
         modify $ Session . M.insert sessionKey (v,timeout,now + fromIntegral timeout*1000) . unSession
         return ()

getSession sessionKey
    = do now <- getTime
         asks $ \s -> case M.lookup sessionKey (unSession s) of
                        Just (v,_,t) | now < t -> Just v
                        _ -> Nothing

setSession sessionKey v
    = modify $ Session . M.adjust (\(_,s,t) -> (v,s,t)) sessionKey . unSession

setSessionTimeout :: key -> SessionKey -> Seconds -> Update (Session key) ()
setSessionTimeout _key sessionKey timeout
    = do now <- getTime
         modify $ Session . M.adjust (\(v,_,_) -> (v,timeout,now+fromIntegral timeout*1000)) sessionKey . unSession
         return (error "setSessionTimeout dummy")

delSession :: key -> SessionKey -> Update (Session key) ()
delSession _key sessionKey
    = do modify $ Session . M.delete sessionKey . unSession
         return (error "delSession dummy")

touchSession :: key -> SessionKey -> Update (Session key) ()
touchSession _key sessionKey
    = do now <- getTime
         modify $ Session . M.adjust (\(v,s,_) -> (v,s,now+fromIntegral s*1000)) sessionKey . unSession


#ifndef __HADDOCK__
$(methods_ SerializeString ''Session ['newSession,'getSession, 'setSession, 'setSessionTimeout, 'delSession, 'touchSession])
$(atStart ''Session [])


$(methods_ SerializeString ''UserComponent [])
$(atStart ''UserComponent [])
#endif
-}
