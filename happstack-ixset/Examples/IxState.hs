{-# LANGUAGE TemplateHaskell , FlexibleInstances, UndecidableInstances, IncoherentInstances, OverlappingInstances,
             MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable,
             TypeFamilies, TypeSynonymInstances, TypeOperators #-}
module IxState where


import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State (modify,put,get,gets)
import Data.Generics hiding ((:+:))
import Happstack.Data
import Happstack.Data.IxSet
import Happstack.State

newtype SesKey = SesKey Integer deriving (Read,Show,Eq,Typeable,Data,Ord,Serialize, Version) 
newtype ETime = ETime Integer deriving (Read,Show,Eq,Typeable,Data,Ord, Serialize, Version) 
data Session val = Session { sessionKey :: SesKey, sessionETime :: ETime, sessionValue :: val}  deriving (Read,Show,Eq,Typeable,Data,Ord) 

instance Version (Session val)
$(deriveSerialize ''Session)

$(deriveNewData [''Session,''SesKey,''ETime])
$(inferIxSet "Sessions" ''Session 'noCalcs [''SesKey,''ETime])

getETime::Update (Sessions val) ETime
getETime = return . ETime =<< getTime

newSession::(Ord val,Data val) => val -> Update (Sessions val) SesKey
newSession val = do
  k <- getRandom 
  let key = SesKey k
  t <- getETime
  modify (insert $ Session key t val)
  return key

getSession::(Data val,Ord val) => 
             SesKey -> Query (Sessions val) (Maybe val)
getSession key = do
  return . liftM sessionValue . getOne . (@=key) =<< ask

setSession key val = do
  t <- getETime
  modify (updateIx key (Session key t val))

cleanSessions :: (Ord val, Data val) => Integer -> Proxy (Sessions val) -> Update (Sessions val) ()
cleanSessions age = proxyUpdate $ do
                      ETime t <- return . ETime =<< getTime
                      let minTime = ETime $ t - age
                      expired <- return . toList . (@<minTime) =<< get
                      mapM (modify . delete) expired
                      return ()

numSessions:: (Ord val) => Proxy (Sessions val) -> Query (Sessions val) Int
numSessions = proxyQuery $ liftM size ask    

$(mkMethods ''Sessions ['newSession
                       ,'setSession
                       --
                       ,'cleanSessions
                       ,'numSessions
                       ,'getSession
                       ])

maintainSessions v = do -- update $ CleanSessions 3600000 v
                         threadDelay (10^6 * 10) -- Once every 10 seconds
                         maintainSessions v 

instance (Serialize (Sessions val), Ord val, Data val, Default val) => Component (Sessions val) where
    type Dependencies (Sessions val) = End
    onLoad proxy = maintainSessions proxy
    initialValue = defaultValue




--------------------------------------------------------------
-- Tests
--------------------------------------------------------------

data EntryPoint = EntryPoint deriving (Typeable)
instance Version EntryPoint
$(deriveSerialize ''EntryPoint)

instance Methods EntryPoint where methods _ = []

instance Component EntryPoint where
    type Dependencies EntryPoint = Sessions String :+:
                                   Sessions Int :+:
                                   End
    initialValue = EntryPoint


runTest :: IO ()
runTest = bracket (startSystemState (proxy EntryPoint)) shutdownSystem $ \ctl ->
          do key1 <- update $ NewSession "5+5="
             key2 <- update $ NewSession (10::Int)

             Just val1 <- query $ GetSession key1
             Just val2 <- query $ GetSession key2

             putStrLn $ val1 ++ show (val2::Int)

             n1 <- query $ NumSessions (Proxy :: Proxy (Sessions String))
             n2 <- query $ NumSessions (Proxy :: Proxy (Sessions Int))
             putStrLn $ "Session entries: " ++ show (n1+n2)

main = runTest
