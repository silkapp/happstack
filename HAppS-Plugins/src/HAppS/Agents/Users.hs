{-# OPTIONS -fth -fglasgow-exts -cpp -fallow-overlapping-instances #-}
-- | This is a convenience module using SessionKeeper.
module HAppS.Agents.Users
{-    (Username, Users, EvUsers,
     -- * User handling
     newUser, modUser, listUsers,
     -- * Sessions
     loginUser, logoffUser,
     getUser, setUserLoginTimeOut
) -} where

{-
import Control.Monad.State
import HAppS.Agents.SessionKeeper
import HAppS.DBMS.RSMap as M
import HAppS.MACID
import HAppS.Util.Cron
import HAppS.Util.Common
import HAppS.Util.StdMain.StartState
import HAppS.Util.StdMain.StartStateTH

type Username = String

data Users d = Users
    { uData :: M.Map Username d
    , uSes  :: SessionState Username
    } deriving(Read,Show)

#ifndef __HADDOCK__
$(inferRecordUpdaters ''Users)
$(inferStartStateAny ''Users)
#endif

type EvUsers a r = forall ev. Ev (Users a) ev r

-- | Create a new user.
newUser :: Username -> d -> EvUsers d ()
newUser un d = do
  ud <- gets uData
  case insertLookupWithKey (\_ _ old -> old) un d ud of
    (Nothing, x) -> modify $ \st -> st { uData = x }
    _            -> fail "user exists already"

-- | Modify a user.
modUser :: Username -> (d -> Maybe d) -> EvUsers d ()
modUser un fun = do
  ud <- gets uData
  case updateLookupWithKey (const fun) un ud of
    (Just _, x)  -> modify $ \st -> st { uData = x }
    _            -> fail "No such user"

-- | List all users in the system.
listUsers :: EvUsers d [(Username,d)]
listUsers = fmap toList $ gets uData

-- | Login an user. Throw an exception in the computation on password failures etc.
loginUser :: Seconds -> Username -> (d -> Ev (Users d) ev res) -> Ev (Users d) ev (SessionKey, res)
loginUser sec un comp = do
  dat <- M.lookup un =<< gets uData
  res <- comp dat
  ses <- withUSes $ newSession sec un
  return (ses,res)

-- | Logoff an user.
logoffUser :: SessionKey -> EvUsers d ()
logoffUser sk = withUSes $ delSession sk

-- | Get a logged in user from a SessionKey.
getUser :: SessionKey -> EvUsers d (Username,d)
getUser sk = do
 un <- withUSes $ getSession sk
 ud <- M.lookup un =<< gets uData
 return (un,ud)

-- | Set new timeout for login.
setUserLoginTimeOut :: SessionKey -> EpochSeconds -> EvUsers d ()
setUserLoginTimeOut sk ep = withUSes $ setSessionTimeOut sk ep
-}
