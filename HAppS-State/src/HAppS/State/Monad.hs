{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE CPP #-}
module HAppS.State.Monad where

import Control.Exception
import Control.Concurrent.STM
import HAppS.State.Types
import HAppS.Data.Proxy

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad

{-
instance (Monad (m STM), MonadTrans m) => Monad (Ev (m STM)) where
    return x = Ev $ return x
    fail x   = unsafeIOToEv (logM "HAppS.State.Monad" CRITICAL ("Ev failure: "++x)) >> Ev (fail x)
    ev >>= f = Ev $ unEv ev >>= unEv . f
-}
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
proxyUpdate f proxy = setUpdateType proxy >> f

-- | Use a proxy to force the type of a query action.
setQueryType :: Proxy t -> Query t ()
setQueryType _ = return ()
proxyQuery f proxy = setQueryType proxy >> f

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

class CatchEv m where
#if __GLASGOW_HASKELL__ < 610
    catchEv :: Ev m a -> (Exception -> a) -> Ev m a
#else
    catchEv :: Ev m a -> (SomeException -> a) -> Ev m a
#endif
instance CatchEv (ReaderT st STM) where
    catchEv (Ev cmd) fun = Ev $ \s -> ReaderT $ \r -> runReaderT (cmd s) r `catchSTM` (\a -> return (fun a))

instance CatchEv (StateT st STM) where
    catchEv (Ev cmd) fun = Ev $ \s -> StateT $ \r -> runStateT (cmd s) r `catchSTM` (\a -> return (fun a,r))

instance MonadPlus m => MonadPlus (Ev m) where
    mzero = Ev $ \_ -> mzero
    mplus (Ev fn1) (Ev fn2) = Ev $ \env -> fn1 env `mplus` fn2 env

{-
-- | Catch errors.
catchEv :: Ev m a -> (Exception -> a) -> Ev m a
catchEv (Ev cmd) fun = Ev $ StateT $ \s -> runStateT cmd s `catchSTM` (\a -> return (fun a, s))
-}
-- | Select a part of the environment.
sel :: (Env -> b) -> AnyEv b
sel f = Ev $ \env -> return (f env)

-- | Run a computation with a local environment.
{-
plocal :: (Env sta a -> Env stb b) -> Ev stb b r -> Ev sta a r
plocal fun (Ev c) = Ev $ StateT $ \s ->
                    do (r,s') <- runStateT c (fun s)
                       return (r,s)
-}
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



{-
localState ifun ufun (Ev cmd) = Ev $ do
    old <- readRefSTM (evState env)
    ntv <- newRefSTM $! ifun old
    res <- cmd $ env { evState = ntv }
    new <- readRefSTM ntv
    writeRefSTM (evState env) $! ufun new old
    return res
-}

-- | Run a computation with local event type.
{-
localEvent :: ev -> Ev st ev a -> Ev st oev a
localEvent ev (Ev cmd) = Ev $ StateT $ \s -> do (r, s') <- runStateT cmd s{evEvent = (evEvent s){txEvent = ev}}
                                                return (r,s'{evEvent = evEvent s})
-}
--    cmd $ env { evEvent = (evEvent env) { txEvent = ev } }
