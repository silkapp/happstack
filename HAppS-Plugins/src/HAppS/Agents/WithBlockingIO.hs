{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -cpp #-}
module HAppS.Agents.WithBlockingIO
{-    (BlockingIOState, withBlockingIO, withBlockingIO_
    ) -} where

{-
import HAppS.MACID
import HAppS.MACID.Logger
import HAppS.MACID.Transaction
import HAppS.MACID.Types
import HAppS.Util.EventHandler
import HAppS.Util.StdMain.StartState
import HAppS.Util.TimeOut

import Data.Typeable
import Control.Concurrent
import Control.Monad.State
import Control.Monad.Trans
import GHC.Exts

import Control.Exception
#ifndef __HADDOCK__
data BlockingIOState dispatch where
    BIOS :: EventHandler dispatch st ev res => Chan (BEvent ev res) -> Ev st ev res -> BlockingIOState dispatch
#else
data BlockingIOState dispatch 
#endif

instance Typeable dispatch => Typeable (BlockingIOState dispatch) where
    typeOf _ = mkTyConApp (mkTyCon "HAppS.Agents.WithBlockingIO.BlockingIOState") [typeOf (undefined :: dispatch)]
    
{-
instance (EventHandler dispatch st ev res, Serialize ev
         ,Typeable ev, Typeable res, EventRelation (BEvent ev res) (IO ())) => StartStateEx st (BlockingIOState dispatch) where
    startStateExM _ = do ch <- liftIO $ newChan
                         return $ BIOS ch $ eventHandler (Proxy :: Proxy dispatch)
    runPartEx     = fmap (:[]) . run

instance (Typeable dispatch, EventHandler dispatch st ev res) => Serialize (BlockingIOState dispatch) where
    typeString _ = "HAppS.Agents.WithBlockingIO.BIOS"
    encodeStringM _ = return ""
    decodeStringM s = do ch <- liftIO $ newChan
                         return (BIOS ch $ eventHandler (Proxy :: Proxy dispatch), s)
-}
instance LogFormat (BEvent ev res) where
    logFormat _ _ = "BEvent from WithBlockingIO"

#ifndef __HADDOCK__
data BEvent ev res where
    BEvent :: ev -> (res -> IO ()) -> BEvent ev res
    BHere  :: Chan (BEvent ev res) -> BEvent ev res
    BNone  ::                         BEvent ev res
#else
data BEvent ev res
#endif

instance (Typeable ev, Typeable res) => Typeable (BEvent ev res) where
    typeOf _ = mkTyConApp (mkTyCon "HAppS.Agents.WithBlockingIO.BEvent") [typeOf (undefined :: ev), typeOf (undefined :: res)]

instance (Typeable ev, Typeable res,Serialize ev) => Serialize (BEvent ev res) where
    typeString x = "HAppS.Agents.WithBlockingIO.BEvent "++typeString (y x)
        where y :: Proxy (BEvent a b) -> Proxy a
              y _ = Proxy
    encodeStringM (BEvent ev _) = liftM ('E':) $ encodeStringM ev
    encodeStringM _             = return "C"
    decodeStringM ('C':rest)    = return (BNone, rest)
    decodeStringM ('E':rest)    = liftM w $ decodeStringM rest
        where w (a,b) = (BEvent a ignore, b)

ignore :: a -> IO ()
ignore _ = return ()


handleBEvent :: EventHandler dispatch s ev res =>
                (s -> BlockingIOState dispatch)
                -> (BlockingIOState dispatch -> s -> s)
                -> BEvent ev res
                -> Ev s (BEvent ev res) (IO ())
handleBEvent i _ (BEvent ev io) = do return . io =<< localEvent ev =<< gets (bioEv . i)
handleBEvent i u (BHere new)    = do evh <- gets (bioEv . i)
                                     old <- gets (bioMv . i)
                                     modify $ u $ BIOS new evh
                                     addSideEffect 30 $ forkIO (wrapper old new) >> return ()
                                     return $ return ()
handleBEvent _ _ (BNone)        = return $ return ()

bioEv :: EventHandler dispatch s ev res => BlockingIOState dispatch -> Ev s ev res
bioEv (BIOS _ evh) = unsafeCoerce# evh

bioMv :: EventHandler dispatch s ev res => BlockingIOState dispatch -> Chan (BEvent ev res)
bioMv (BIOS mv _) = unsafeCoerce# mv

wrapper old new = readChan old >>= writeChan new >> wrapper old new

run :: forall dispatch st ev res. 
       (EventHandler dispatch st ev res, Serialize ev
       ,Typeable ev, Typeable res)
    => (st -> BlockingIOState dispatch)
    -> (BlockingIOState dispatch -> st -> st)
    -> Handler st
run i u = error "withBlockingIO: FIXME" -- (gev :=> (getEvent >>= handleBEvent i u))
    where gev :: Runner (BEvent ev res) (IO ())
          gev = do ch <- newChan
                   writeChan ch $ BHere ch
                   return (readChan ch, id)


withBlockingIO :: EventHandler dispatch st ev res => Seconds -> IO ev -> Ev (BlockingIOState dispatch) anyev (IO res)
withBlockingIO sec _ | sec > 1000 = fail ("withBlockingIO: too long timeout "++show sec)
withBlockingIO sec action = do
  mv <- unsafeIOToEv $ newEmptyMVar
  ch <- gets bioMv
  addSideEffect (sec+300) $ do
    res <- withSafeTimeOut (sec * second) action
    writeChan ch $ BEvent res (\e -> putMVar mv e >> putMVar mv e)
    takeMVar mv
    return ()
  return $ takeMVar mv

withBlockingIO_ :: EventHandler dispatch st ev res => Seconds -> IO ev -> Ev (BlockingIOState dispatch) anyev ()
withBlockingIO_ sec _ | sec > 1000 = fail ("withBlockingIO: too long timeout "++show sec)
withBlockingIO_ sec action = do
  ch <- gets bioMv
  addSideEffect (sec+300) $ handle print $ do
    mv  <- newEmptyMVar
    res <- withSafeTimeOut (sec * second) action
    writeChan ch $ BEvent res (\_ -> putMVar mv ())
    takeMVar mv
  return ()

-}

