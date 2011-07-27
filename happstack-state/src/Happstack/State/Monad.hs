{-# LANGUAGE CPP, FlexibleInstances, TypeSynonymInstances,
    MultiParamTypeClasses #-}
module Happstack.State.Monad where

import Control.Exception
import Control.Concurrent.STM
import Happstack.State.Types
import Happstack.Data.Proxy

import Control.Monad.State
import Control.Monad.Reader

instance (Monad m) => Monad (Ev m) where
    return x = Ev $ \_ -> return x
    fail x   = Ev $ \_ -> fail x
    ev >>= f = Ev $ \env -> unEv ev env >>= \x -> unEv (f x) env

instance MonadState st (Update st) where
    get   = Ev $ \_ -> get
    put x = Ev $ \_ -> put x

instance MonadReader st (Query st) where
    ask = Ev $ \_ -> ask
    local l (Ev cmd) = Ev $ \env -> local l (cmd env)

instance MonadReader st (Update st) where
    ask = Ev $ \_ -> get
    local l (Ev cmd) = Ev $ \env -> StateT $ \s ->
                       do (r,_s') <- runStateT (cmd env) (l s)
                          return (r,s)

instance (Monad m) => Functor (Ev m) where fmap = liftM

-- | Use a proxy to force the type of an update action.
setUpdateType :: Proxy t -> Update t ()
setUpdateType _ = return ()

-- | Forces the type of the proxy and update to match
proxyUpdate :: Update t b -> Proxy t -> Update t b
proxyUpdate f prox = setUpdateType prox >> f

-- | Use a proxy to force the type of a query action.
setQueryType :: Proxy t -> Query t ()
setQueryType _ = return ()

-- | Forces the type of proxy and query to match
proxyQuery :: Query t b -> Proxy t -> Query t b
proxyQuery f prox = setQueryType prox >> f

-- | Currying version of 'setUpdateType'.
asUpdate :: Update t a -> Proxy t -> Update t a
asUpdate upd _ = upd

-- | Currying version of 'setQueryType'.
asQuery :: Query t a -> Proxy t -> Query t a
asQuery query _ = query

-- | Specialized version of 'ask'
askState :: Query st st
askState = ask

-- | Specialized version of 'get'
getState :: Update st st
getState = get

-- | Specialized version of 'put'.
putState :: st -> Update st ()
putState = put


-- | Lift an STM action into Ev.
liftSTM :: STM a -> AnyEv a
liftSTM = unsafeSTMToEv

{-
In stm >= 2.2.0.1 catchSTM always has the type:

catchSTM :: Exception e => STM a -> (e -> STM a) -> STM a

In earlier versions of stm the type of catchSTM depends on what is exported by GHC.Conc.catchSTM from base.
-}
class CatchEv m where
#if (MIN_VERSION_stm(2,2,0))
    catchEv :: (Exception e) => Ev m a -> (e -> a) -> Ev m a
#else
#if (MIN_VERSION_base(4,3,0))
    catchEv :: (Exception e) => Ev m a -> (e -> a) -> Ev m a
#else
#if (MIN_VERSION_base(4,0,0))
    catchEv :: Ev m a -> (SomeException -> a) -> Ev m a
#else
    catchEv :: Ev m a -> (Exception -> a) -> Ev m a
#endif
#endif
#endif
instance CatchEv (ReaderT st STM) where
    catchEv (Ev cmd) fun = Ev $ \s -> ReaderT $ \r -> runReaderT (cmd s) r `catchSTM` (\a -> return (fun a))

instance CatchEv (StateT st STM) where
    catchEv (Ev cmd) fun = Ev $ \s -> StateT $ \r -> runStateT (cmd s) r `catchSTM` (\a -> return (fun a,r))

instance MonadPlus m => MonadPlus (Ev m) where
    mzero = Ev $ \_ -> mzero
    mplus (Ev fn1) (Ev fn2) = Ev $ \env -> fn1 env `mplus` fn2 env

-- | Select a part of the environment.
sel :: (Env -> b) -> AnyEv b
sel f = Ev $ \env -> return (f env)

-- FIXME: should the users see this function?
-- | Run a computation with local state. Changes to state will be visible to outside.
localState :: (outer -> inner) -> (inner -> outer -> outer) -> Ev (StateT inner STM) a -> Ev (StateT outer STM) a
localState ifun ufun (Ev cmd)
    = Ev $ \env -> StateT $ \s ->
      do (r,s') <- runStateT (cmd env) (ifun s)
         return (r, ufun s' s)

-- | Run a computation with local state.
localStateReader :: (outer -> inner) -> Ev (ReaderT inner STM) a -> Ev (ReaderT outer STM) a
localStateReader ifun (Ev cmd)
    = Ev $ \env -> ReaderT $ \s ->
      runReaderT (cmd env) (ifun s)

-- | Execute a Query action in the Update monad.
runQuery :: Query st a -> Update st a
runQuery fn = Ev $ \env -> StateT $ \st ->
              do a <- runReaderT (unEv fn env) st
                 return (a,st)
