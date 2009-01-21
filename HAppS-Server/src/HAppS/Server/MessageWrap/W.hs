{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances -fno-monomorphism-restriction #-}
module HAppS.Server.MessageWrap.W
{--    (w, wIO, wPure, ehShow, throwToMessage,
     ToHttpResultM, ToHttpResultPure, Wrap(..),
     postProcessMIO, postProcessMPure,
     toHttpResultIO, toHttpResultPure,failT
    ,xGET,xPOST,oPOST,oGET,xmlWrapM,retVal,retVal'
    ,oGET',oPOST',xGET',xPOST',ioGET,xGET'',ext2,fun',ext3,w',o,io
    )
--}
 where

import HAppS.MACID
import HAppS.Server.HTTP.Types as H
import HAppS.Server.HTTP.ServerPart
import HAppS.Server.MessageWrap
import HAppS.Server.MinHaXML
import Control.Exception
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Dynamic

{-

data DynErr = forall msg. ToMessage msg => DynErr msg
            deriving(Typeable)

throwToMessage :: ToMessage msg => msg -> a
throwToMessage msg = throwDyn (DynErr msg)


newtype Wrap t = Wrap { unWrap :: t }


class ToHttpResultM a where toHttpResultM' :: a -> Ev st rq (IO Result)



instance ToHttpResultM (Wrap Result)  where toHttpResultM' = return . return . unWrap
instance ToHttpResultM (Wrap (IO Result))  where toHttpResultM' = return . unWrap

instance ToElement el => ToHttpResultM (Wrap (XML el))      where toHttpResultM' = return . toMessageM . unWrap
instance ToElement el => ToHttpResultM (Wrap (IO (XML el))) where toHttpResultM' (Wrap x) = return (x >>= toMessageM)

{--instance ToElement element => ToHttpResultM (Wrap (IO element)) where
  toHttpResultM' (Wrap x) = return (x >>= xmlMessageWrap)
--}
--instance ToElement element => ToHttpResultM (Wrap element) where
--  toHttpResultM' = fmap return . xmlMessageWrap . unWrap

class ToHttpResultPure a where toHttpResult' :: a -> Ev st rq Result

instance ToHttpResultPure (Wrap Result)  where toHttpResult' = return . unWrap
instance ToElement el => ToHttpResultPure (Wrap (XML el)) where toHttpResult' = toMessageM . unWrap
instance ToElement el => ToHttpResultPure (Wrap el) where
  toHttpResult' = xmlMessageWrap . unWrap

xmlMessageWrap ev  = toMessageM $ XML NoStyle $ toElement ev


toHttpResultPure :: ToHttpResultPure (Wrap res) => res -> Ev st rq Result
toHttpResultPure = toHttpResult' . Wrap

toHttpResultIO :: ToHttpResultM (Wrap res) => res -> Ev st rq (IO Result)
toHttpResultIO = toHttpResultM' . Wrap


type MFun = forall m. Monad m => Result -> m Result

postProcessMIO :: ToHttpResultM (Wrap res) => MFun -> Ev st rq res -> Ev st rq (IO Result)
postProcessMIO mf act = do msg <- act
                           res <- toHttpResultM' (Wrap msg)
                           return (res >>= mf)

postProcessMPure :: ToHttpResultPure (Wrap res) => MFun -> Ev st rq res -> Ev st rq Result
postProcessMPure mf act = act >>= (toHttpResult' . Wrap) >>= mf

-- | IO \"w\" combinator combined with ehShow. 
--   Handles decoding input, encoding output and prettyprinting answers.
--   Answers may be in pure or inside IO.
--w :: forall input result st. (FromMessage input, ToHttpResultM (Wrap result)) => (input -> Ev st Request result) -> Ev st Request (IO H.Result)
w = wIO ehShow

-- | IO \"w\" combinator. Handles decoding input, encoding output and prettyprinting answers.
--   Answers may be in pure or inside IO.
wIO :: forall input result st. (FromMessage input, ToHttpResultM (Wrap result)) =>
       (Exception -> IO H.Result) -> (input -> Ev st Request result) -> Ev st Request (IO H.Result)
wIO eh fun  = work `catchEv` eh
    where work :: Ev st Request (IO H.Result)
          work = getEvent >>= fromMessageM >>= fun >>= toHttpResultM' . Wrap

-- | Pure \"w\" combinator. Handles decoding input, encoding output and prettyprinting answers.
wPure :: forall input result st. (FromMessage input, ToHttpResultPure (Wrap result)) =>
         (Exception -> Identity H.Result) -> (input -> Ev st Request result) -> Ev st Request H.Result
wPure eh fun = work `catchEv` (runIdentity . eh)
    where work :: Ev st Request H.Result
          work = getEvent >>= fromMessageM >>= fun >>= toHttpResult' . Wrap


-- | Show errors and handle 'DynErr' with the 'ToMessage' instance.
ehShow :: (Functor m, Monad m) => Exception -> m H.Result
ehShow (DynException e) | Just (DynErr v) <- fromDynamic e = toMessageM v
ehShow e = let r="403: "++(show e) in
    fmap (setHeader "Content-Type" "text/plain") $ sresult 403 r


xmlWrapM x = toMessageM $ (?style::Element -> XML Element) $ toElement x
o x = io (\() -> x)
io fun = w' fun
w' fun = wIO ehShow' $ \x-> fun x >>= xmlWrapM


xGET fun = (GET,w' fun)
xPOST fun = (POST, w' fun)
oPOST foo = (POST,w foo)
oGET foo = (GET,w foo)
ioGET foo = (GET,foo)
retVal x () = return x

retVal' x _ () = return x
retVal'::(Monad m) => a -> t -> () -> m a

xGET' fun = (GET, w' . fun)
xPOST' fun = (POST, w' . fun)
oGET' fun = (GET, w . fun)
oPOST' fun = (POST,w . fun)

type PathInfo = [String]
{-
xGET'' fun pathInfo req =
    do
    let method = rqMethod req
    if method/=GET then return mzero else 
       ext2 result >>= (return . return)
    where
    f=fun pathInfo
    result = w'' f
    ext2 x =  
        do f<-ask
           liftIO $ f x req
    w'' f = getEvent >>= fromMessageM >>= f >>= toMsg
    toMsg = return . toMessageM . (?style::Element -> XML Element) . toElement 

xGET''::(
         -- Monad m 
         --,Functor m,

        --MonadReader (Ev st Request (m Result) -> Request -> IO Result) IO,


        ?style::Element -> XML Element
        ,FromMessage input,ToElement result
        ) => 
        (PathInfo -> input -> Ev st Request result) 
            -> 
        PathInfo -> Request -> EvParIO st (Maybe Result)
        --Ev st Request (Maybe (m Result))
-}

--type OpType input result m = 


--(PathInfo -> input -> m result)
--type XFun input result m = 
--        XFun input result m
--(Ev st Request) 

--w'' fun = wIO ehShow' $ \x-> fun x >>= xmlWrapM'
--xmlWrapM' x = toMessageM $ (?style::Element -> XML Element) $ toElement x




{--type XFun = forall input result m.
       (FromMessage input,ToElement result) =>
       (PathInfo -> input -> m result )
--}
-- ToHttpResultM (Wrap result)
--Ev st Request (m result)

--Ev st Request (IO H.Result) 
{--
type XFun = forall input result st m. 
       (FromMessage input, ToHttpResultM (Wrap result),ToElement result,Monad m) =>
       (PathInfo -> input -> Ev st Request (m result))
--}


--wIO :: (FromMessage input, ToHttpResultM (Wrap result)) =>
--       (Exception -> IO H.Result) -> (input -> Ev st Request result) -> 
--        Ev st Request (IO H.Result)

fun' pi () = return $ sresult 404 "asd"

--ext2 :: ((Ev st Request res) -> (Ev st Request res)) -> (Request -> Ev st Request Result)
--ext2 ::  EvParIO res st


--ext3 x = Handle (ext2 x)

{--    
    
--}
    --return $ Just $ (io . fun) pathInfo
             

--instance (ToElement x) => ToMessage x where
--    toMessageM = xmlWrapM'

--ehShow' :: (Functor m, Monad m) => Exception -> m H.Result
ehShow' (DynException e) | Just (DynErr' v) <- fromDynamic e = xmlWrapM v
ehShow' e = let r="403: "++(show e) in
    fmap (setHeader "Content-Type" "text/plain") $ sresult 403 r

data DynErr' = forall msg. ToElement msg => DynErr' msg
               deriving(Typeable)


failT e = throwDyn $ DynErr' e


-}