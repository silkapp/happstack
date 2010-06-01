
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Happstack.Server.SimpleHTTP
-- Copyright   :  (c) Happstack.com 2009; (c) HAppS Inc 2007
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@vo.com
-- Stability   :  provisional
-- Portability :  requires mtl
--
-- 'SimpleHTTP' provides a back-end independent API for handling HTTP
-- requests.
--
-- By default, the built-in HTTP server will be used. However, other
-- back-ends like CGI\/FastCGI can be used if so desired.
--
-- So the general nature of 'simpleHTTP' is just what you'd expect
-- from a web application container.  First you figure out which
-- function is going to process your request, process the request to
-- generate a response, then return that response to the client. The
-- web application container is started with 'simpleHTTP', which takes
-- a configuration and a response-building structure ('ServerPartT'
-- which I'll return to in a moment), picks the first handler that is
-- willing to accept the request, and passes the request in to the
-- handler.  A simple \"hello world\" style Happstack 'simpleHTTP'
-- server looks like:
--
-- >  main = simpleHTTP nullConf $ return "Hello World!"
--
-- @simpleHTTP nullConf@ creates a HTTP server on port 8000.  return
-- \"Hello World!\" creates a 'ServerPartT' that just returns that
-- text.
--
-- 'ServerPartT' is the basic response builder.  As you might expect,
-- it's a container for a function that takes a 'Request' and converts
-- it to a response suitable for sending back to the server.  Most of
-- the time though you don't even need to worry about that as
-- 'ServerPartT' hides almost all the machinery for building your
-- response by exposing a few type classes.
--
-- 'ServerPartT' is a pretty rich monad.  You can interact with your
-- 'Request', your 'Response', do 'IO', etc.  Here is a do block that
-- validates basic authentication.  It takes a realm name as a string,
-- a Map of username to password and a server part to run if
-- authentication fails.
--
-- 'basicAuth' acts like a guard, and only produces a response when
-- authentication fails.  So put it before any 'ServerPartT' for which
-- you want to demand authentication, in any collection of
-- 'ServerPartT's.
--
-- > main = simpleHTTP nullConf $ myAuth, return "Hello World!"
-- >     where
-- >         myAuth = basicAuth' "Test"
-- >             (M.fromList [("hello", "world")]) (return "Login Failed")
--
-- > basicAuth' realmName authMap unauthorizedPart =
-- >    do
-- >        let validLogin name pass = M.lookup name authMap == Just pass
-- >        let parseHeader = break (':'==) . Base64.decode . B.unpack . B.drop 6
-- >        authHeader <- getHeaderM "authorization"
-- >        case authHeader of
-- >            Nothing -> err
-- >            Just x  -> case parseHeader x of
-- >                (name, ':':pass) | validLogin name pass -> mzero
-- >                                   | otherwise -> err
-- >                _                                       -> err
-- >    where
-- >        err = do
-- >            unauthorized ()
-- >            setHeaderM headerName headerValue
-- >            unauthorizedPart
-- >        headerValue = "Basic realm=\"" ++ realmName ++ "\""
-- >        headerName  = "WWW-Authenticate"
--
-- Here is another example that uses 'liftIO' to embed IO in a request process:
--
-- >  main = simpleHTTP nullConf $ myPart
-- >  myPart = do
-- >    line <- liftIO $ do -- IO
-- >        putStr "return? "
-- >        getLine
-- >    when (take 2 line /= "ok") $ (notfound () >> return "refused")
-- >    return "Hello World!"
--
-- This example will ask in the console \"return? \" if you type \"ok\" it will
-- show \"Hello World!\" and if you type anything else it will return a 404.
--
-----------------------------------------------------------------------------
module Happstack.Server.SimpleHTTP
    ( module Happstack.Server.HTTP.Types
    , module Happstack.Server.Cookie
    -- * SimpleHTTP
    , simpleHTTP
    , simpleHTTP'
    , simpleHTTP''
    , simpleHTTPWithSocket
    , simpleHTTPWithSocket'
    , bindPort
    , parseConfig
    -- * ServerPartT
    , ServerPartT(..)
    , ServerPart
    , runServerPartT
    , mapServerPartT
    , mapServerPartT'
    , withRequest
    , anyRequest
    -- * WebT
    , WebT(..)
    , UnWebT
    , FilterFun
    , Web
    , mkWebT
    , ununWebT
    , runWebT
    , mapWebT
    -- * Type Classes
    , FromReqURI(..)
    , ToMessage(..)
    , toResponseBS
      -- * Manipulating requests
    , FromData(..)
    , ServerMonad(..)
    , RqData
    , noHandle
    , getHeaderM
    , escape
    , escape'
    , multi
      -- * Manipulating responses
    , FilterMonad(..)
    , ignoreFilters
    , SetAppend(..)
    , FilterT(..)
    , WebMonad(..)
    , addCookie
    , addCookies
    , expireCookie
    , addHeaderM
    , setHeaderM
    , ifModifiedSince
    , modifyResponse
    , setResponseCode
    , resp

      -- * Respond Codes
    , ok
    , badGateway
    , internalServerError
    , badRequest
    , unauthorized
    , forbidden
    , notFound
    , seeOther
    , found
    , movedPermanently
    , tempRedirect

     -- * guards and building blocks
    , guardRq
    , dir
    , dirs
    , host
    , withHost
    , method
    , methodSP
    , methodM
    , methodOnly
    , nullDir
    , path
    , anyPath
    , anyPath'
    , trailingSlash
    , withData
    , withDataFn
    , getDataFn
    , getData
    , require
    , requireM
    , basicAuth
    , uriRest
    , flatten
    , localContext
      -- * proxying
    , proxyServe
    , rproxyServe
      -- * unknown
    , debugFilter
    , applyRequest
      -- * Parsing input and cookies
    , lookInput   -- :: String -> Data Input
    , lookBS      -- :: String -> Data B.ByteString
    , look        -- :: String -> Data String
    , lookCookie  -- :: String -> Data Cookie
    , lookCookieValue -- :: String -> Data String
    , readCookieValue -- :: Read a => String -> Data a
    , lookRead    -- :: Read a => String -> Data a
    , lookPairs
      -- * XSLT
    , xslt ,doXslt
      -- * Error Handlng
    , errorHandlerSP
    , simpleErrorHandler
    , spUnwrapErrorT
      -- * Output Validation
    , setValidator
    , setValidatorSP
    , validateConf
    , runValidator
    , wdgHTMLValidator
    , noopValidator
    , lazyProcValidator
    ) where

import Happstack.Server.HTTP.Types hiding (Version(..))
import Happstack.Server.Cookie

import qualified Paths_happstack_server          as Cabal

import qualified Data.Version                    as DV
import Happstack.Server.HTTP.Client              (getResponse, unproxify, unrproxify)
import Happstack.Data.Xml.HaXml                  (toHaXmlEl)
import qualified Happstack.Server.MinHaXML       as H
import qualified Happstack.Server.HTTP.Listen    as Listen (listen, listen',listenOn) -- So that we can disambiguate 'Writer.listen'
import Happstack.Server.XSLT                     (XSLTCmd, XSLPath, procLBSIO)
import Happstack.Server.SURI                     (ToSURI)
import Happstack.Util.Common                     (Seconds, readM)
import Happstack.Data                            (Xml, normalize, fromPairs, Element, toXml, toPublicXml) -- used by default implementation of fromData
import Network                                   (Socket)
import Control.Applicative                       (Applicative, pure, (<*>), Alternative(empty,(<|>)))
import Control.Concurrent                        (forkIO)
import Control.Exception                         (evaluate)
import Control.Monad                             ( MonadPlus, mzero, mplus
                                                 , msum, ap, unless
                                                 , liftM, liftM2, liftM3, liftM4
                                                 )
import Control.Monad.Trans                       ( MonadTrans, lift
                                                 , MonadIO, liftIO
                                                 )
import Control.Monad.Reader                      ( ReaderT(ReaderT), runReaderT
                                                 , MonadReader, ask, local
                                                 , asks
                                                 )
import Control.Monad.Writer                      ( WriterT(WriterT), runWriterT
                                                 , MonadWriter, tell, pass
                                                 , listens
                                                 )
import qualified Control.Monad.Writer            as Writer (listen) -- So that we can disambiguate 'Listen.listen'
import Control.Monad.State                       (MonadState, get, put)
import Control.Monad.Error                       ( ErrorT(ErrorT), runErrorT
                                                 , Error, strMsg
                                                 , MonadError, throwError, catchError
                                                 , mapErrorT
                                                 )
import Control.Monad.Maybe                       (MaybeT(MaybeT), runMaybeT)
import Data.Char                                 (ord)
import Data.Maybe                                (fromMaybe)
import Data.Monoid                               ( Monoid, mempty, mappend
                                                 , Dual(Dual), getDual
                                                 , Endo(Endo), appEndo
                                                 )

import qualified Data.ByteString.Char8           as B
import qualified Data.ByteString.Lazy.Char8      as L
import qualified Data.ByteString.Lazy.UTF8       as LU (toString, fromString)

import qualified Data.Generics                   as G
import qualified Data.Map                        as M

import Text.Html                                 (Html, renderHtml)
import qualified Text.XHtml                      as XHtml (Html, renderHtml)

import qualified Happstack.Crypto.Base64         as Base64
import Data.Char                                 (toLower)
import Data.List                                 (isPrefixOf,stripPrefix,tails,inits)
import System.IO                                 (hGetContents, hClose)
import System.Console.GetOpt                     ( OptDescr(Option)
                                                 , ArgDescr(ReqArg)
                                                 , ArgOrder(Permute)
                                                 , getOpt
                                                 )
import System.Locale                             (defaultTimeLocale)
import System.Process                            (runInteractiveProcess, waitForProcess)
import System.Time                               (CalendarTime, formatCalendarTime)
import System.Exit                               (ExitCode(ExitSuccess, ExitFailure))
import System.FilePath                           (makeRelative, splitDirectories)
import Debug.Trace                               (trace)

-- | An alias for 'WebT' when using 'IO'.
type Web a = WebT IO a
-- | An alias for using 'ServerPartT' when using the 'IO'.
type ServerPart a = ServerPartT IO a

--------------------------------------
-- HERE BEGINS ServerPartT definitions

-- | 'ServerPartT' is a container for processing requests and returning results.
newtype ServerPartT m a = ServerPartT { unServerPartT :: ReaderT Request (WebT m) a }
    deriving (Monad, MonadIO, MonadPlus, Functor)

-- | Particularly useful when combined with 'runWebT' to produce
-- a @m ('Maybe' 'Response')@ from a 'Request'.
runServerPartT :: ServerPartT m a -> Request -> WebT m a
runServerPartT = runReaderT . unServerPartT

withRequest :: (Request -> WebT m a) -> ServerPartT m a
withRequest = ServerPartT . ReaderT

-- | Used to manipulate the containing monad.  Very useful when
-- embedding a monad into a 'ServerPartT', since 'simpleHTTP' requires
-- a @'ServerPartT' 'IO' a@.  Refer to 'WebT' for an explanation of
-- the structure of the monad.
--
-- Here is an example.  Suppose you want to embed an 'ErrorT' into your
-- 'ServerPartT' to enable 'throwError' and 'catchError' in your 'Monad'.
--
-- > type MyServerPartT e m a = ServerPartT (ErrorT e m) a
--
-- Now suppose you want to pass @MyServerPartT@ into a function that
-- demands a @'ServerPartT' 'IO' a@ (e.g. 'simpleHTTP').  You can
-- provide the function:
--
-- >   unpackErrorT :: (Monad m, Show e) => UnWebT (ErrorT e m) a -> UnWebT m a
-- >   unpackErrorT et = do
-- >      eitherV <- runErrorT et
-- >      return $ case eitherV of
-- >          Left err -> Just (Left $ toResponse $ 
-- >                                   "Catastrophic failure " ++ show err
-- >                           , Set $ Dual $ Endo $ \r -> r{rsCode = 500})
-- >          Right x -> x
--
-- With @unpackErrorT@ you can now call 'simpleHTTP'. Just wrap your
-- 'ServerPartT' list.
--
-- >  simpleHTTP nullConf $ mapServerPartT unpackErrorT (myPart `catchError` myHandler)
--
-- Or alternatively:
--
-- >  simpleHTTP' unpackErrorT nullConf (myPart `catchError` myHandler)
--
-- Also see 'spUnwrapErrorT' for a more sophisticated version of this
-- function.
--
mapServerPartT :: (     UnWebT m a ->      UnWebT n b)
               -> (ServerPartT m a -> ServerPartT n b)
mapServerPartT f ma = withRequest $ \rq -> mapWebT f (runServerPartT ma rq)

-- | A variant of 'mapServerPartT' where the first argument also takes
-- a 'Request'.  Useful if you want to 'runServerPartT' on a different
-- 'ServerPartT' inside your monad (see 'spUnwrapErrorT').
mapServerPartT' :: (Request -> UnWebT m a ->      UnWebT n b)
                -> (      ServerPartT m a -> ServerPartT n b)
mapServerPartT' f ma = withRequest $ \rq -> mapWebT (f rq) (runServerPartT ma rq)

instance MonadTrans (ServerPartT) where
    lift m = withRequest (\_ -> lift m)

instance (Monad m) => Monoid (ServerPartT m a) where
    mempty  = mzero
    mappend = mplus

instance (Monad m, Functor m) => Applicative (ServerPartT m) where
    pure = return
    (<*>) = ap

instance (Functor m, MonadPlus m) => Alternative (ServerPartT m) where
    empty = mzero
    (<|>) = mplus

instance (Monad m, MonadWriter w m) => MonadWriter w (ServerPartT m) where
    tell = lift . tell
    listen m = withRequest $ \rq ->  Writer.listen (runServerPartT m rq) >>= return
    pass m = withRequest $ \rq -> pass (runServerPartT m rq) >>= return

instance (Monad m, MonadError e m) => MonadError e (ServerPartT m) where
    throwError e = lift $ throwError e
    catchError action handler = withRequest $ \rq -> (runServerPartT action rq) `catchError` ((flip runServerPartT $ rq) . handler)

instance (Monad m, MonadReader r m) => MonadReader r (ServerPartT m) where
    ask = lift ask
    local fn m = withRequest $ \rq-> local fn (runServerPartT m rq)

instance Monad m => FilterMonad Response (ServerPartT m) where
    setFilter = anyRequest . setFilter
    composeFilter = anyRequest . composeFilter
    getFilter m = withRequest $ \rq -> getFilter (runServerPartT m rq)

instance Monad m => WebMonad Response (ServerPartT m) where
    finishWith r = anyRequest $ finishWith r

-- | Yes, this is exactly like 'ReaderT' with new names.  Why you ask?
-- Because 'ServerPartT' can lift up a 'ReaderT'.  If you did that, it
-- would shadow 'ServerPartT''s behavior as a 'ReaderT', thus meaning if
-- you lifted the 'ReaderT' you could no longer ask for the 'Request'.
-- This way you can add a 'ReaderT' to your monad stack without any
-- trouble.
class Monad m => ServerMonad m where
    askRq   :: m Request
    localRq :: (Request -> Request) -> m a -> m a

instance (Monad m) => ServerMonad (ServerPartT m) where
    askRq = ServerPartT $ ask
    localRq f m = ServerPartT $ local f (unServerPartT m)

instance (Error e, ServerMonad m) => ServerMonad (ErrorT e m) where
    askRq     = lift askRq
    localRq f = mapErrorT $ localRq f

-------------------------------
-- HERE BEGINS WebT definitions

-- | A monoid operation container.  If @a@ is a monoid, then
-- 'SetAppend' is a monoid with the following behaviors:
--
-- >  Set    x `mappend` Append y = Set    (x `mappend` y)
-- >  Append x `mappend` Append y = Append (x `mappend` y)
-- >  _        `mappend` Set y    = Set y
--
-- A simple way of summarizing this is, if the right side is 'Append',
-- then the right is appended to the left.  If the right side is
-- 'Set', then the left side is ignored.

data SetAppend a = Set a | Append a
    deriving (Eq, Show)

instance Monoid a => Monoid (SetAppend a) where
   mempty = Append mempty

   Set    x `mappend` Append y = Set    (x `mappend` y)
   Append x `mappend` Append y = Append (x `mappend` y)
   _        `mappend` Set y    = Set y

-- | Extract the value from a 'SetAppend'.
-- Note that a 'SetAppend' is actually a @CoPointed@ from:
-- <http://hackage.haskell.org/packages/archive/category-extras/latest/doc/html/Control-Functor-Pointed.html>
-- But lets not drag in that dependency. yet...
extract :: SetAppend t -> t
extract (Set    x) = x
extract (Append x) = x

instance Functor (SetAppend) where
    fmap f (Set    x) = Set    $ f x
    fmap f (Append x) = Append $ f x

-- | 'FilterFun' is a lot more fun to type than @'SetAppend' ('Dual'
-- ('Endo' a))@.
type FilterFun a = SetAppend (Dual (Endo a))

unFilterFun :: FilterFun a -> (a -> a)
unFilterFun = appEndo . getDual . extract

newtype FilterT a m b = FilterT { unFilterT :: WriterT (FilterFun a) m b }
   deriving (Monad, MonadTrans, Functor, MonadIO)

-- | A set of functions for manipulating filters.  A 'ServerPartT'
-- implements 'FilterMonad' 'Response' so these methods are the
-- fundamental ways of manipulating the response object, especially
-- before you've converted your monadic value to a 'Response'.
class Monad m => FilterMonad a m | m->a where
    -- | Ignores all previous alterations to your filter
    --
    -- As an example:
    --
    -- > do
    -- >   composeFilter f
    -- >   setFilter g
    -- >   return "Hello World"
    --
    -- The @'setFilter' g@ will cause the first @'composeFilter' f@ to
    -- be ignored.
    setFilter :: (a->a) -> m ()
    -- | Composes your filter function with the existing filter
    -- function.
    composeFilter :: (a->a) -> m ()
    -- | Retrieves the filter from the environment.
    getFilter :: m b -> m (b, a->a)

instance (Monad m) => FilterMonad a (FilterT a m) where
    setFilter     = FilterT . tell                . Set    . Dual . Endo
    composeFilter = FilterT . tell                . Append . Dual . Endo
    getFilter     = FilterT . listens unFilterFun . unFilterT

-- | The basic 'Response' building object.
newtype WebT m a = WebT { unWebT :: ErrorT Response (FilterT (Response) (MaybeT m)) a }
    deriving (MonadIO, Functor)

-- | It is worth discussing the unpacked structure of 'WebT' a bit as
--  it's exposed in 'mapServerPartT' and 'mapWebT'.
--
--  A fully unpacked 'WebT' has a structure that looks like:
--
--  > ununWebT $ WebT m a :: m (Maybe (Either Response a, FilterFun Response))
--
--  So, ignoring @m@, as it is just the containing 'Monad', the
--  outermost layer is a 'Maybe'.  This is 'Nothing' if 'mzero' was
--  called or @'Just' ('Either' 'Response' a, 'SetAppend' ('Endo'
--  'Response'))@ if 'mzero' wasn't called.  Inside the 'Maybe', there
--  is a pair.  The second element of the pair is our filter function
--  @'FilterFun' 'Response'@.  @'FilterFun' 'Response'@ is a type
--  alias for @'SetAppend' ('Dual' ('Endo' 'Response'))@.  This is
--  just a wrapper for a @'Response' -> 'Response'@ function with a
--  particular 'Monoid' behavior.  The value
--
--  >  Append (Dual (Endo f))
--
--  Causes @f@ to be composed with the previous filter.
--
--  >  Set (Dual (Endo f))
--
--  Causes @f@ to not be composed with the previous filter.
--
--  Finally, the first element of the pair is either @'Left'
--  'Response'@ or @'Right' a@.
--
--  Another way of looking at all these pieces is from the behaviors
--  they control.  The 'Maybe' controls the 'mzero' behavior.  @'Set'
--  ('Endo' f)@ comes from the 'setFilter' behavior.  Likewise,
--  @'Append' ('Endo' f)@ is from 'composeFilter'.  @'Left'
--  'Response'@ is what you get when you call 'finishWith' and
--  @'Right' a@ is the normal exit.
--
--  An example case statement looks like:
--
--  >  ex1 webt = do
--  >    val <- ununWebT webt
--  >    case val of
--  >        Nothing -> Nothing  -- this is the interior value when mzero was used
--  >        Just (Left r, f) -> Just (Left r, f) -- r is the value that was passed into "finishWith"
--  >                                             -- f is our filter function
--  >        Just (Right a, f) -> Just (Right a, f) -- a is our normal monadic value
--  >                                               -- f is still our filter function
--
type UnWebT m a = m (Maybe (Either Response a, FilterFun Response))

instance Monad m => Monad (WebT m) where
    m >>= f = WebT $ unWebT m >>= unWebT . f
    return a = WebT $ return a
    fail s = outputTraceMessage s (mkFailMessage s)

instance Error Response where
    strMsg = toResponse

class Monad m => WebMonad a m | m->a where
    -- | A control structure.  It ends the computation and returns the
    -- 'Response' you passed into it immediately.  This provides an
    -- alternate escape route.  In particular it has a monadic value
    -- of any type.  And unless you call @'setFilter' id@ first your
    -- response filters will be applied normally.
    --
    -- Extremely useful when you're deep inside a monad and decide
    -- that you want to return a completely different content type,
    -- since it doesn't force you to convert all your return types to
    -- 'Response' early just to accommodate this.
    finishWith :: a -> m b

instance (Monad m) => WebMonad Response (WebT m) where
    finishWith r = WebT $ throwError r

instance MonadTrans WebT where
    lift = WebT . lift . lift . lift

instance (Monad m) => MonadPlus (WebT m) where
    -- | Aborts a computation.
    --
    -- This is primarily useful because 'msum' will take an array of
    -- 'MonadPlus' and return the first one that isn't 'mzero', which
    -- is exactly the semantics expected from objects that take lists
    -- of 'ServerPartT'.
    mzero = WebT $ lift $ lift $ mzero
    mplus x y =  WebT $ ErrorT $ FilterT $ (lower x) `mplus` (lower y)
        where lower = (unFilterT . runErrorT . unWebT)

-- | Deprecated: use 'mzero'.
noHandle :: (MonadPlus m) => m a
noHandle = mzero
{-# DEPRECATED noHandle "Use mzero" #-}

instance (Monad m) => FilterMonad Response (WebT m) where
    setFilter f = WebT $ lift $ setFilter $ f
    composeFilter f = WebT . lift . composeFilter $ f
    getFilter     m = WebT $ ErrorT $ fmap lft $ getFilter (runErrorT $ unWebT m)
        where
          lft (Left  r, _) = Left r
          lft (Right a, f) = Right (a, f)

instance (Monad m) => Monoid (WebT m a) where
    mempty = mzero
    mappend = mplus

-- | Takes your 'WebT', if it is 'mempty' it returns 'Nothing' else it
-- converts the value to a 'Response' and applies your filter to it.
runWebT :: forall m b. (Functor m, ToMessage b) => WebT m b -> m (Maybe Response)
runWebT = (fmap . fmap) appFilterToResp . ununWebT
    where
      appFilterToResp :: (Either Response b, FilterFun Response) -> Response
      appFilterToResp (e, ff) = unFilterFun ff $ either id toResponse e

-- | For when you really need to unpack a 'WebT' entirely (and not
-- just unwrap the first layer with 'unWebT').
ununWebT :: WebT m a -> UnWebT m a
ununWebT = runMaybeT . runWriterT . unFilterT . runErrorT . unWebT

-- | For wrapping a 'WebT' back up.  @'mkWebT' . 'ununWebT' = 'id'@
mkWebT :: UnWebT m a -> WebT m a
mkWebT = WebT . ErrorT . FilterT . WriterT . MaybeT

-- | See 'mapServerPartT' for a discussion of this function.
mapWebT :: (UnWebT m a -> UnWebT n b)
        -> (  WebT m a ->   WebT n b)
mapWebT f ma = mkWebT $ f (ununWebT ma)

instance (Monad m, Functor m) => Applicative (WebT m) where
    pure = return
    (<*>) = ap

instance (Functor m, MonadPlus m) => Alternative (WebT m) where
    empty = mzero
    (<|>) = mplus

instance MonadReader r m => MonadReader r (WebT m) where
    ask = lift ask
    local fn m = mkWebT $ local fn (ununWebT m)

instance MonadState st m => MonadState st (WebT m) where
    get = lift get
    put = lift . put

instance MonadError e m => MonadError e (WebT m) where
	throwError err = lift $ throwError err
 	catchError action handler = mkWebT $ catchError (ununWebT action) (ununWebT . handler)

instance MonadWriter w m => MonadWriter w (WebT m) where
    tell = lift . tell
    listen m = mkWebT $ Writer.listen (ununWebT m) >>= (return . liftWebT)
        where liftWebT (Nothing, _) = Nothing
              liftWebT (Just (Left x,f), _) = Just (Left x,f)
              liftWebT (Just (Right x,f),w) = Just (Right (x,w),f)
    pass m = mkWebT $ ununWebT m >>= liftWebT
        where liftWebT Nothing = return Nothing
              liftWebT (Just (Left x,f)) = return $ Just (Left x, f)
              liftWebT (Just (Right x,f)) = pass (return x)>>= (\a -> return $ Just (Right a,f))

-- | An alias for @'setFilter' 'id'@. It resets all your filters.
ignoreFilters :: (FilterMonad a m) => m ()
ignoreFilters = setFilter id

-- | Used to ignore all your filters and immediately end the
-- computation.  A combination of 'ignoreFilters' and 'finishWith'.
escape :: (WebMonad a m, FilterMonad a m) => m a -> m b
escape gen = ignoreFilters >> gen >>= finishWith

-- | An alternate form of 'escape' that can be easily used within a do
-- block.
escape' :: (WebMonad a m, FilterMonad a m) => a -> m b
escape' a = ignoreFilters >> finishWith a

----------------------------------------------
-- additional types


-- | An array of 'OptDescr', useful for processing command line
-- options into an 'Conf' for 'simpleHTTP'.
ho :: [OptDescr (Conf -> Conf)]
ho = [Option [] ["http-port"] (ReqArg (\h c -> c { port = read h }) "port") "port to bind http server"]

-- | Parse command line options into a 'Conf'.
parseConfig :: [String] -> Either [String] Conf
parseConfig args
    = case getOpt Permute ho args of
        (flags,_,[]) -> Right $ foldr ($) nullConf flags
        (_,_,errs)   -> Left errs

-- | Use the built-in web-server to serve requests according to a
-- 'ServerPartT'.  Use 'msum' to pick the first handler from a list of
-- handlers that doesn't call 'mzero'. This function always binds o
-- IPv4 ports until Network module is fixed to support IPv6 in a
-- portable way. Use 'simpleHTTPWithSocket' with custom socket if you
-- want different behaviour.
simpleHTTP :: (ToMessage a) => Conf -> ServerPartT IO a -> IO ()
simpleHTTP = simpleHTTP' id

-- | A combination of 'simpleHTTP''' and 'mapServerPartT'.  See
-- 'mapServerPartT' for a discussion of the first argument of this
-- function. This function always binds to IPv4 ports until Network
-- module is fixed to support IPv6 in a portable way. Use
-- 'simpleHTTPWithSocket' with custom socket if you want different
-- behaviour.
simpleHTTP' :: (ToMessage b, Monad m, Functor m) => (UnWebT m a -> UnWebT IO b)
            -> Conf -> ServerPartT m a -> IO ()
simpleHTTP' toIO conf hs =
    Listen.listen conf (\req -> runValidator (fromMaybe return (validator conf)) =<< (simpleHTTP'' (mapServerPartT toIO hs) req))


-- | Generate a result from a 'ServerPartT' and a 'Request'. This is
-- mainly used by CGI (and fast-cgi) wrappers.
simpleHTTP'' :: (ToMessage b, Monad m, Functor m) => ServerPartT m b -> Request -> m Response
simpleHTTP'' hs req =  (runWebT $ runServerPartT hs req) >>= (return . (maybe standardNotFound id))
    where
        standardNotFound = setHeader "Content-Type" "text/html" $ (toResponse notFoundHtml){rsCode=404}

-- | Run 'simpleHTTP' with a previously bound socket. Useful if you
-- want to run happstack as user on port 80. Use something like this:
--
-- > import System.Posix.User (setUserID, UserEntry(..), getUserEntryForName)
-- >
-- > main = do
-- >     let conf = nullConf { port = 80 }
-- >     socket <- bindPort conf
-- >     -- do other stuff as root here
-- >     getUserEntryForName "www" >>= setUserID . userID
-- >     -- finally start handling incoming requests
-- >     tid <- forkIO $ simpleHTTPWithSocket socket conf impl
--
-- Note: It's important to use the same conf (or at least the same
-- port) for 'bindPort' and 'simpleHTTPWithSocket'.
simpleHTTPWithSocket :: (ToMessage a) => Socket -> Conf -> ServerPartT IO a -> IO ()
simpleHTTPWithSocket = simpleHTTPWithSocket' id

-- | Like 'simpleHTTP'' with a socket.
simpleHTTPWithSocket' :: (ToMessage b, Monad m, Functor m) => (UnWebT m a -> UnWebT IO b)
                      -> Socket -> Conf -> ServerPartT m a -> IO ()
simpleHTTPWithSocket' toIO socket conf hs =
    Listen.listen' socket conf (\req -> runValidator (fromMaybe return (validator conf)) =<< (simpleHTTP'' (mapServerPartT toIO hs) req))

-- | Bind port and return the socket for 'simpleHTTPWithSocket'. This
-- function always binds to IPv4 ports until Network module is fixed
-- to support IPv6 in a portable way.
bindPort :: Conf -> IO Socket
bindPort conf = Listen.listenOn (port conf)

-- | This class is used by 'path' to parse a path component into a
-- value.  At present, the instances for number types ('Int', 'Float',
-- etc) just call 'readM'. The instance for 'String' however, just
-- passes the path component straight through. This is so that you can
-- read a path component which looks like this as a 'String'
-- \/somestring\/ instead of requiring the path component to look like
-- \/\"somestring\"\/.
class FromReqURI a where
    fromReqURI :: String -> Maybe a

instance FromReqURI String  where fromReqURI = Just
instance FromReqURI Int     where fromReqURI = readM
instance FromReqURI Integer where fromReqURI = readM
instance FromReqURI Float   where fromReqURI = readM
instance FromReqURI Double  where fromReqURI = readM

type RqData a = ReaderT ([(String,Input)], [(String,Cookie)]) Maybe a

-- | Useful for 'withData' and 'getData''. Make your preferred data
-- type an instance of 'FromData' to use those functions.
class FromData a where
    fromData :: RqData a

instance (Eq a,Show a,Xml a,G.Data a) => FromData a where
    fromData = do mbA <- lookPairs >>= return . normalize . fromPairs
                  case mbA of
                    Just a -> return a
                    Nothing -> fail "FromData G.Data failure"
--    fromData = lookPairs >>= return . normalize . fromPairs

instance (FromData a, FromData b) => FromData (a,b) where
    fromData = liftM2 (,) fromData fromData
instance (FromData a, FromData b, FromData c) => FromData (a,b,c) where
    fromData = liftM3 (,,) fromData fromData fromData
instance (FromData a, FromData b, FromData c, FromData d) => FromData (a,b,c,d) where
    fromData = liftM4 (,,,) fromData fromData fromData fromData
instance FromData a => FromData (Maybe a) where
    fromData = fmap Just fromData `mplus` return Nothing

-- | A low-level function to build a 'Response' from a content-type
-- and a 'ByteString'.
--
-- Creates a 'Response' in a manner similar to the 'ToMessage' class,
-- but without requiring an instance declaration.
toResponseBS :: B.ByteString -- ^ content-type
             -> L.ByteString -- ^ response body
             -> Response
toResponseBS contentType message =
    let res = Response 200 M.empty nullRsFlags message Nothing
    in setHeaderBS (B.pack "Content-Type") contentType res


-- | Used to convert arbitrary types into an HTTP response.  You need
-- to implement this if you want to pass @'ServerPartT' m@ containing
-- your type into 'simpleHTTP'.
--
-- Minimal definition: 'toMessage'.
class ToMessage a where
    toContentType :: a -> B.ByteString
    toContentType _ = B.pack "text/plain"
    toMessage :: a -> L.ByteString
    toMessage = error "Happstack.Server.SimpleHTTP.ToMessage.toMessage: Not defined"
    toResponse:: a -> Response
    toResponse val =
        let bs = toMessage val
            res = Response 200 M.empty nullRsFlags bs Nothing
        in setHeaderBS (B.pack "Content-Type") (toContentType val)
           res

instance ToMessage [Element] where
    toContentType _ = B.pack "application/xml; charset=UTF-8"
    toMessage [el] = LU.fromString $ H.simpleDoc H.NoStyle $ toHaXmlEl el -- !! OPTIMIZE
    toMessage x    = error ("Happstack.Server.SimpleHTTP 'instance ToMessage [Element]' Can't handle " ++ show x)




instance ToMessage () where
    toContentType _ = B.pack "text/plain"
    toMessage () = L.empty
instance ToMessage String where
    toContentType _ = B.pack "text/plain; charset=UTF-8"
    toMessage = LU.fromString
instance ToMessage Integer where
    toMessage = toMessage . show
instance ToMessage a => ToMessage (Maybe a) where
    toContentType _ = toContentType (undefined :: a)
    toMessage Nothing = toMessage "nothing"
    toMessage (Just x) = toMessage x


instance ToMessage Html where
    toContentType _ = B.pack "text/html; charset=UTF-8"
    toMessage = LU.fromString . renderHtml

instance ToMessage XHtml.Html where
    toContentType _ = B.pack "text/html; charset=UTF-8"
    toMessage = LU.fromString . XHtml.renderHtml

instance ToMessage Response where
    toResponse = id
{-

-- This instances causes awful error messages. I am removing it and
-- seeing if anyone complains. I doubt they will.

instance (Xml a)=>ToMessage a where
    toContentType = toContentType . toXml
    toMessage = toMessage . toPublicXml
-}

--    toMessageM = toMessageM . toPublicXml


class MatchMethod m where matchMethod :: m -> Method -> Bool
instance MatchMethod Method where matchMethod m = (== m)
instance MatchMethod [Method] where matchMethod methods = (`elem` methods)
instance MatchMethod (Method -> Bool) where matchMethod f = f
instance MatchMethod () where matchMethod () _ = True

-- | The function 'flatten' turns your arbitrary @m a@ and converts it
-- too a @m 'Response'@ with 'toResponse'.
flatten :: (ToMessage a, Functor f) => f a -> f Response
flatten = fmap toResponse

-- | This is kinda like a very oddly shaped 'mapServerPartT' or 'mapWebT'.
-- You probably want one or the other of those.
localContext :: Monad m => (WebT m a -> WebT m' a) -> ServerPartT m a -> ServerPartT m' a
localContext fn hs
    = withRequest $ \rq -> fn (runServerPartT hs rq)


-- | Get a header out of the request.
getHeaderM :: (ServerMonad m) => String -> m (Maybe B.ByteString)
getHeaderM a = askRq >>= return . (getHeader a)

-- | Add headers into the response.  This method does not overwrite
-- any existing header of the same name, hence the name 'addHeaderM'.
-- If you want to replace a header use 'setHeaderM'.
addHeaderM :: (FilterMonad Response m) => String -> String -> m ()
addHeaderM a v = composeFilter $ \res-> addHeader a v res

-- | Set a header into the response.  This will replace an existing
-- header of the same name.  Use 'addHeaderM' if you want to add more
-- than one header of the same name.
setHeaderM :: (FilterMonad Response m) => String -> String -> m ()
setHeaderM a v = composeFilter $ \res -> setHeader a v res
-------------------------------------
-- guards

-- | Guard using an arbitrary function on the 'Request'.
guardRq :: (ServerMonad m, MonadPlus m) => (Request -> Bool) -> m ()
guardRq f = do
    rq <- askRq
    unless (f rq) mzero

-- | Guard against the method. This function also guards against
-- any remaining path segments. See 'methodOnly' for the version
-- that guards only by method.
methodM :: (ServerMonad m, MonadPlus m, MatchMethod method) => method -> m ()
methodM meth = methodOnly meth >> nullDir

-- | Guard against the method only (as opposed to 'methodM').
methodOnly :: (ServerMonad m, MonadPlus m, MatchMethod method) => method -> m ()
methodOnly meth = guardRq $ \rq -> matchMethod meth (rqMethod rq)

-- | Guard against the method. Note, this function also guards against
-- any remaining path segments.
methodSP :: (ServerMonad m, MonadPlus m, MatchMethod method) => method -> m b-> m b
methodSP m handle = methodM m >> handle

-- | Guard against the method. Note, this function also guards against any
-- remaining path segments.  This function is deprecated.  You can probably
-- just use 'methodSP' (or 'methodM') now.
method :: (MatchMethod method, Monad m) => method -> WebT m a -> ServerPartT m a
method m handle = methodSP m (anyRequest handle)
{-# DEPRECATED method "you should be able to use methodSP" #-}

-- | Guard against non-empty remaining path segments.
nullDir :: (ServerMonad m, MonadPlus m) => m ()
nullDir = guardRq $ \rq -> null (rqPaths rq)

-- | Pop a path element and run the 'ServerPartT' if it matches the
-- given string.
-- 
-- The path element can not contain \'/\'. See also 'dirs'.
dir :: (ServerMonad m, MonadPlus m) => String -> m a -> m a
dir staticPath handle =
    do
        rq <- askRq
        case rqPaths rq of
            (p:xs) | p == staticPath -> localRq (\newRq -> newRq{rqPaths = xs}) handle
            _ -> mzero
            
-- | Guard against a 'FilePath'. Unlike 'dir' the 'FilePath' may
-- contain \'/\'. If the guard succeeds, the matched elements will be
-- popped from the directory stack.
--
-- > dirs "foo/bar" $ ...
--          
-- See also: 'dir'.
dirs :: (ServerMonad m, MonadPlus m) => FilePath -> m a -> m a 
dirs fp m = 
     do let parts = splitDirectories (makeRelative "/" fp) 
        foldr dir m parts

-- | Guard against the host.
host :: (ServerMonad m, MonadPlus m) => String -> m a -> m a
host desiredHost handle =
    do rq <- askRq
       case getHeader "host" rq of
         (Just hostBS) | desiredHost == B.unpack hostBS -> handle
         _ -> mzero

-- | Lookup the host header and pass it to the handler.
withHost :: (ServerMonad m, MonadPlus m) => (String -> m a) -> m a
withHost handle =
    do rq <- askRq
       case getHeader "host" rq of
         (Just hostBS) -> handle (B.unpack hostBS)
         _ -> mzero


-- | Pop a path element and parse it using the 'fromReqURI' in the
-- 'FromReqURI' class.
path :: (FromReqURI a, MonadPlus m, ServerMonad m) => (a -> m b) -> m b
path handle = do
    rq <- askRq
    case rqPaths rq of
        (p:xs) | Just a <- fromReqURI p
                            -> localRq (\newRq -> newRq{rqPaths = xs}) (handle a)
        _ -> mzero

-- | Grab the rest of the URL (dirs + query) and passes it to your
-- handler.
uriRest :: (ServerMonad m) => (String -> m a) -> m a
uriRest handle = askRq >>= handle . rqURL

-- | Pop any path element and ignore when choosing a 'ServerPartT' to
-- handle the request.
anyPath :: (ServerMonad m, MonadPlus m) => m r -> m r
anyPath x = path $ (\(_::String) -> x)

-- | Deprecated: use 'anyPath'.
anyPath' :: (ServerMonad m, MonadPlus m) => m r -> m r
anyPath' = anyPath
{-# DEPRECATED anyPath' "Use anyPath" #-}

-- | Guard which checks that the Request URI ends in @\'\/\'@.  Useful
-- for distinguishing between @foo@ and @foo/@
trailingSlash :: (ServerMonad m, MonadPlus m) => m ()
trailingSlash = guardRq $ \rq -> (last (rqUri rq)) == '/'

-- | Parse your request with a 'RqData' (a 'ReaderT', basically) For
-- example here is a simple @GET@ or @POST@ variable based
-- authentication guard.  It handles the request with 'errorHandler'
-- if authentication fails.
--
-- > myRqData = do
-- >     username <- lookInput "username"
-- >     password <- lookInput "password"
-- >     return (username, password)
-- > checkAuth errorHandler = do
-- >     d <- getData myRqDataA
-- >     case d of
-- >         Nothing -> errorHandler
-- >         Just a | isValid a -> mzero
-- >         Just a | otherwise -> errorHandler
getDataFn :: (ServerMonad m) => RqData a -> m (Maybe a)
getDataFn rqData = do
    rq <- askRq
    return $ runReaderT rqData (rqInputs rq, rqCookies rq)

-- | An variant of 'getData' that uses 'FromData' to chose your
-- 'RqData' for you.  The example from 'getData' becomes:
--
-- >  myRqData = do
-- >     username <- lookInput "username"
-- >     password <- lookInput "password"
-- >     return (username, password)
-- >  instance FromData (String,String) where
-- >     fromData = myRqData
-- >  checkAuth errorHandler = do
-- >     d <- getData'
-- >     case d of
-- >         Nothing -> errorHandler
-- >         Just a | isValid a -> mzero
-- >         Just a | otherwise -> errorHandler
--
getData :: (ServerMonad m, FromData a) => m (Maybe a)
getData = getDataFn fromData

-- | Retrieve data from the input query or the cookies.
withData :: (FromData a, MonadPlus m, ServerMonad m) => (a -> m r) -> m r
withData = withDataFn fromData

-- | The 'withDataFn' is like 'withData', but you pass in a 'RqData'
-- monad for reading.
withDataFn :: (MonadPlus m, ServerMonad m) => RqData a -> (a -> m r) -> m r
withDataFn fn handle = getDataFn fn >>= maybe mzero handle

-- | The 'proxyServe' is for creating 'ServerPartT's that proxy.  The
-- sole argument @['String']@ is a list of allowed domains for
-- proxying.  This matches the domain part of the request and the
-- wildcard * can be used. E.g.
--
--  * \"*\" to match anything.
--
--  * \"*.example.com\" to match anything under example.com
--
--  * \"example.com\" to match just example.com
--
--
--  TODO: annoyingly enough, this method eventually calls 'escape', so
--  any headers you set won't be used, and the computation immediately
--  ends.
proxyServe :: (MonadIO m, WebMonad Response m, ServerMonad m, MonadPlus m, FilterMonad Response m) => [String] -> m Response
proxyServe allowed = do
   rq <- askRq
   if cond rq then proxyServe' rq else mzero
   where
   cond rq
     | "*" `elem` allowed = True
     | domain `elem` allowed = True
     | superdomain `elem` wildcards =True
     | otherwise = False
     where
     domain = head (rqPaths rq)
     superdomain = tail $ snd $ break (=='.') domain
     wildcards = (map (drop 2) $ filter ("*." `isPrefixOf`) allowed)

-- | Take a proxy 'Request' and create a 'Response'.  Your basic proxy
-- building block.  See 'unproxify'.
--
-- TODO: this would be more useful if it didn\'t call 'escape'
-- (e.g. it let you modify the response afterwards, or set additional
-- headers)
proxyServe' :: (MonadIO m, FilterMonad Response m, WebMonad Response m) => Request-> m Response
proxyServe' rq = liftIO (getResponse (unproxify rq)) >>=
                either (badGateway . toResponse . show) escape'

-- | This is a reverse proxy implementation.  See 'unrproxify'.
--
-- TODO: this would be more useful if it didn\'t call 'escape', just
-- like 'proxyServe''.
rproxyServe :: (MonadIO m) =>
    String -- ^ defaultHost
    -> [(String, String)] -- ^ map to look up hostname mappings.  For the reverse proxy
    -> ServerPartT m Response -- ^ the result is a 'ServerPartT' that will reverse proxy for you.
rproxyServe defaultHost list  = withRequest $ \rq ->
                liftIO (getResponse (unrproxify defaultHost list rq)) >>=
                either (badGateway . toResponse . show) (escape')

-- | Run an 'IO' action and, if it returns 'Just', pass it to the
-- second argument.
require :: (MonadIO m, MonadPlus m) => IO (Maybe a) -> (a -> m r) -> m r
require fn handle = do
    mbVal <- liftIO fn
    case mbVal of
        Nothing -> mzero
        Just a -> handle a

-- | A variant of require that can run in any monad, not just 'IO'.
requireM :: (MonadTrans t, Monad m, MonadPlus (t m)) => m (Maybe a) -> (a -> t m r) -> t m r
requireM fn handle = do
    mbVal <- lift fn
    case mbVal of
        Nothing -> mzero
        Just a -> handle a

-- | Use @cmd@ to transform XML against @xslPath@.  This function only
-- acts if the content-type is @application\/xml@.
xslt :: (MonadIO m, MonadPlus m, ToMessage r) =>
        XSLTCmd  -- ^ XSLT preprocessor. Usually 'xsltproc' or 'saxon'.
     -> XSLPath      -- ^ Path to xslt stylesheet.
     -> m r -- ^ Affected 'ServerPart's.
     -> m Response
xslt cmd xslPath parts = do
    res <- parts
    if toContentType res == B.pack "application/xml"
        then liftM toResponse (doXslt cmd xslPath (toResponse res))
        else return (toResponse res)

doXslt :: (MonadIO m) =>
          XSLTCmd -> XSLPath -> Response -> m Response
doXslt cmd xslPath res =
    do new <- liftIO $ procLBSIO cmd xslPath $ rsBody res
       return $ setHeader "Content-Type" "text/html" $
              setHeader "Content-Length" (show $ L.length new) $
              res { rsBody = new }

-- | Add the cookie with a timeout to the response.
addCookie :: (FilterMonad Response m) => Seconds -> Cookie -> m ()
addCookie sec = (addHeaderM "Set-Cookie") . mkCookieHeader sec

-- | Add the list of cookie timeout pairs to the response.
addCookies :: (FilterMonad Response m) => [(Seconds, Cookie)] -> m ()
addCookies = mapM_ (uncurry addCookie)

-- | Expire the cookie immediately.
expireCookie :: (FilterMonad Response m) => String -> m () 
expireCookie cookieName = addCookie 0 (mkCookie cookieName "")

-- |Honor an @if-modified-since@ header in a 'Request'.
-- If the 'Request' includes the @if-modified-since@ header and the
-- 'Response' has not been modified, then return 304 (Not Modified),
-- otherwise return the 'Response'.
ifModifiedSince :: CalendarTime -- ^ mod-time for the 'Response' (MUST NOT be later than server's time of message origination)
                -> Request -- ^ incoming request (used to check for if-modified-since)
                -> Response -- ^ Response to send if there are modifications
                -> Response
ifModifiedSince modTime request response =
    let repr = formatCalendarTime defaultTimeLocale "%a, %d %b %Y %X GMT" modTime
        notmodified = getHeader "if-modified-since" request == Just (B.pack $ repr)
    in if notmodified
          then result 304 "" -- Not Modified
          else setHeader "Last-modified" repr response

-- | Deprecated:  use 'composeFilter'.
modifyResponse :: (FilterMonad a m) => (a -> a) -> m()
modifyResponse = composeFilter
{-# DEPRECATED modifyResponse "Use composeFilter" #-}

-- | Set the return code in your response.
setResponseCode :: FilterMonad Response m => Int -> m ()
setResponseCode code
    = composeFilter $ \r -> r{rsCode = code}

-- | Same as @'setResponseCode' status >> return val@.
resp :: (FilterMonad Response m) => Int -> b -> m b
resp status val = setResponseCode status >> return val

-- | Respond with @200 OK@.
ok :: (FilterMonad Response m) => a -> m a
ok = resp 200

-- | Respond with @500 Internal Server Error@.
internalServerError :: (FilterMonad Response m) => a -> m a
internalServerError = resp 500

-- | Responds with @502 Bad Gateway@.
badGateway :: (FilterMonad Response m) => a -> m a
badGateway = resp 502

-- | Respond with @400 Bad Request@.
badRequest :: (FilterMonad Response m) => a -> m a
badRequest = resp 400

-- | Respond with @401 Unauthorized@.
unauthorized :: (FilterMonad Response m) => a -> m a
unauthorized = resp 401

-- | Respond with @403 Forbidden@.
forbidden :: (FilterMonad Response m) => a -> m a
forbidden = resp 403

-- | Respond with @404 Not Found@.
notFound :: (FilterMonad Response m) => a -> m a
notFound = resp 404

-- | Respond with @303 See Other@.
seeOther :: (FilterMonad Response m, ToSURI uri) => uri -> res -> m res
seeOther uri res = do modifyResponse $ redirect 303 uri
                      return res

-- | Respond with @302 Found@.
found :: (FilterMonad Response m, ToSURI uri) => uri -> res -> m res
found uri res = do modifyResponse $ redirect 302 uri
                   return res

-- | Respond with @301 Moved Permanently@.
movedPermanently :: (FilterMonad Response m, ToSURI a) => a -> res -> m res
movedPermanently uri res = do modifyResponse $ redirect 301 uri
                              return res

-- | Respond with @307 Temporary Redirect@.
tempRedirect :: (FilterMonad Response m, ToSURI a) => a -> res -> m res
tempRedirect val res = do modifyResponse $ redirect 307 val
                          return res

-- | Deprecated: use 'msum'.
multi :: Monad m => [ServerPartT m a] -> ServerPartT m a
multi = msum
{-# DEPRECATED multi "Use msum instead" #-}

-- | What is this for, exactly?  I don't understand why @Show a@ is
-- even in the context Deprecated: This function appears to do nothing
-- at all. If it use it, let us know why.
debugFilter :: (MonadIO m, Show a) => ServerPartT m a -> ServerPartT m a
debugFilter handle =
    withRequest $ \rq -> do
                    r <- runServerPartT handle rq
                    return r

-- | A constructor for a 'ServerPartT' when you don't care about the
-- request.
anyRequest :: Monad m => WebT m a -> ServerPartT m a
anyRequest x = withRequest $ \_ -> x

-- | Again, why is this useful?  Deprecated: No idea why this function
-- would be useful. If you use it, please tell us.
applyRequest :: (ToMessage a, Monad m, Functor m) =>
                ServerPartT m a -> Request -> Either (m Response) b
applyRequest hs = simpleHTTP'' hs >>= return . Left

-- | A simple HTTP basic authentication guard.
basicAuth :: (WebMonad Response m, ServerMonad m, FilterMonad Response m, MonadPlus m) =>
   String -- ^ the realm name
   -> M.Map String String -- ^ the username password map
   -> m a -- ^ the part to guard
   -> m a
basicAuth realmName authMap xs = basicAuthImpl `mplus` xs
  where
    basicAuthImpl = do
        aHeader <- getHeaderM "authorization"
        case aHeader of
            Nothing -> err
            Just x -> case parseHeader x of
                (name, ':':password) | validLogin name password -> mzero
                                     | otherwise -> err
                _  -> err
    validLogin name password = M.lookup name authMap == Just password
    parseHeader = break (':'==) . Base64.decode . B.unpack . B.drop 6
    headerName  = "WWW-Authenticate"
    headerValue = "Basic realm=\"" ++ realmName ++ "\""
    err = escape $ do
            setHeaderM headerName headerValue
            unauthorized $ toResponse "Not authorized"

--------------------------------------------------------------
-- Query/Post data validating
--------------------------------------------------------------

-- | Useful inside the 'RqData' monad.  Gets the named input parameter
-- (either from a @POST@ or a @GET@ request).
lookInput :: String -> RqData Input
lookInput name
    = do inputs <- asks fst
         case lookup name inputs of
           Nothing -> fail "input not found"
           Just i  -> return i

-- | Get the named input parameter as a 'L.ByteString'.
lookBS :: String -> RqData L.ByteString
lookBS = fmap inputValue . lookInput

-- | Get the named input as a 'String'.
look :: String -> RqData String
look = fmap LU.toString . lookBS

-- | Get the named cookie.  The cookie name is case insensitive.
lookCookie :: String -> RqData Cookie
lookCookie name
    = do cookies <- asks snd
         case lookup (map toLower name) cookies of -- keys are lowercased
           Nothing -> fail "cookie not found"
           Just c  -> return c

-- | Get the named cookie as a 'String'.
lookCookieValue :: String -> RqData String
lookCookieValue = fmap cookieValue . lookCookie

-- | Get the named cookie as the requested 'Read' type.
readCookieValue :: Read a => String -> RqData a
readCookieValue name = readM =<< fmap cookieValue (lookCookie name)

-- | Like 'look', but 'Read's for you.
lookRead :: Read a => String -> RqData a
lookRead name = readM =<< look name

-- | Get all the input parameters and convert them to a 'String'.
lookPairs :: RqData [(String,String)]
lookPairs = asks fst >>= return . map (\(n,vbs)->(n,LU.toString $ inputValue vbs))


--------------------------------------------------------------
-- Error Handling
--------------------------------------------------------------

-- | This 'ServerPart' modifier enables the use of 'throwError' and
-- 'catchError' inside the 'WebT' actions, by adding the 'ErrorT'
-- monad transformer to the stack.
--
-- You can wrap the complete second argument to 'simpleHTTP' in this
-- function.
--
errorHandlerSP :: (Monad m, Error e) => (Request -> e -> WebT m a) -> ServerPartT (ErrorT e m) a -> ServerPartT m a
errorHandlerSP handler sps = withRequest $ \req -> mkWebT $ do
			eer <- runErrorT $ ununWebT $ runServerPartT sps req
			case eer of
				Left err -> ununWebT (handler req err)
				Right res -> return res
{-# DEPRECATED errorHandlerSP "Use spUnwrapErrorT" #-}

-- | An example error Handler to be used with 'spUnWrapErrorT', which
-- returns the error message as a plain text message to the browser.
--
-- Another possibility is to store the error message, e.g. as a
-- FlashMsg, and then redirect the user somewhere.
simpleErrorHandler :: (Monad m) => String -> ServerPartT m Response
simpleErrorHandler err = ok $ toResponse $ ("An error occured: " ++ err)

-- | This is a for use with 'mapServerPartT'' It it unwraps the
-- interior monad for use with 'simpleHTTP'.  If you have a
-- @'ServerPartT' ('ErrorT' e m) a@, this will convert that monad into
-- a @'ServerPartT' m a@.  Used with 'mapServerPartT'' to allow
-- 'throwError' and 'catchError' inside your monad.  Eg.
--
-- > simpleHTTP conf $ mapServerPartT' (spUnWrapErrorT failurePart)  $ myPart `catchError` errorPart
--
-- Note that @failurePart@ will only be run if @errorPart@ threw an
-- error so it doesn\'t have to be very complex.
spUnwrapErrorT:: Monad m => (e -> ServerPartT m a)
              -> Request
              -> UnWebT (ErrorT e m) a
              -> UnWebT m a
spUnwrapErrorT handler rq = \x -> do
    err <- runErrorT x
    case err of
        Left e -> ununWebT $ runServerPartT (handler e) rq
        Right a -> return a

--------------------------------------------------------------
-- * Output validation
--------------------------------------------------------------

-- | Set the validator which should be used for this particular
-- 'Response' when validation is enabled.
--
-- Calling this function does not enable validation. That can only be
-- done by enabling the validation in the 'Conf' that is passed to
-- 'simpleHTTP'.
--
-- You do not need to call this function if the validator set in
-- 'Conf' does what you want already.
--
-- Example: (use 'noopValidator' instead of the default supplied by
-- 'validateConf')
--
-- > simpleHTTP validateConf . anyRequest $ ok . setValidator noopValidator =<< htmlPage
--
-- See also: 'validateConf', 'wdgHTMLValidator', 'noopValidator',
-- 'lazyProcValidator'.
setValidator :: (Response -> IO Response) -> Response -> Response
setValidator v r = r { rsValidator = Just v }

-- | 'ServerPart' version of 'setValidator'.
--
-- Example: (Set validator to 'noopValidator')
--
-- >  simpleHTTP validateConf $ setValidatorSP noopValidator (dir "ajax" ... )
--
setValidatorSP :: (Monad m, ToMessage r) => (Response -> IO Response) -> m r -> m Response
setValidatorSP v sp = return . setValidator v . toResponse =<< sp

-- | Extend 'nullConf' by enabling validation and setting
-- 'wdgHTMLValidator' as the default validator for @text\/html@.
--
-- Example:
--
-- > simpleHTTP validateConf . anyRequest $ ok htmlPage
--
validateConf :: Conf
validateConf = nullConf { validator = Just wdgHTMLValidator }

-- | Actually perform the validation on a 'Response'.
--
-- Run the validator specified in the 'Response'. If none is provide
-- use the supplied default instead.
--
-- Note: This function will run validation unconditionally. You
-- probably want 'setValidator' or 'validateConf'.
runValidator :: (Response -> IO Response) -> Response -> IO Response
runValidator defaultValidator r =
    case rsValidator r of
      Nothing -> defaultValidator r
      (Just altValidator) -> altValidator r

-- | Validate @text\/html@ content with @WDG HTML Validator@.
--
-- This function expects the executable to be named @validate@ and it
-- must be in the default @PATH@.
--
-- See also: 'setValidator', 'validateConf', 'lazyProcValidator'.
wdgHTMLValidator :: (MonadIO m, ToMessage r) => r -> m Response
wdgHTMLValidator = liftIO . lazyProcValidator "validate" ["-w","--verbose","--charset=utf-8"] Nothing Nothing handledContentTypes . toResponse
    where
      handledContentTypes (Just ct) = elem (takeWhile (\c -> c /= ';' && c /= ' ') (B.unpack ct)) [ "text/html", "application/xhtml+xml" ]
      handledContentTypes Nothing = False

-- | A validator which always succeeds.
--
-- Useful for selectively disabling validation. For example, if you
-- are sending down HTML fragments to an AJAX application and the
-- default validator only understands complete documents.
noopValidator :: Response -> IO Response
noopValidator = return

-- | Validate the 'Response' using an external application.
--
-- If the external application returns 0, the original response is
-- returned unmodified. If the external application returns non-zero,
-- a 'Response' containing the error messages and original response
-- body is returned instead.
--
-- This function also takes a predicate filter which is applied to the
-- content-type of the response. The filter will only be applied if
-- the predicate returns true.
--
-- NOTE: This function requires the use of -threaded to avoid
-- blocking.  However, you probably need that for Happstack anyway.
--
-- See also: 'wdgHTMLValidator'.
lazyProcValidator :: FilePath -- ^ name of executable
               -> [String] -- ^ arguments to pass to the executable
               -> Maybe FilePath -- ^ optional path to working directory
               -> Maybe [(String, String)] -- ^ optional environment (otherwise inherit)
               -> (Maybe B.ByteString -> Bool) -- ^ content-type filter
               -> Response -- ^ Response to validate
               -> IO Response
lazyProcValidator exec args wd env mimeTypePred response
    | mimeTypePred (getHeader "content-type" response) =
        do (inh, outh, errh, ph) <- runInteractiveProcess exec args wd env
           out <- hGetContents outh
           err <- hGetContents errh
           forkIO $ do L.hPut inh (rsBody response)
                       hClose inh
           forkIO $ evaluate (length out) >> return ()
           forkIO $ evaluate (length err) >> return ()
           ec <- waitForProcess ph
           case ec of
             ExitSuccess     -> return response
             (ExitFailure _) ->
                 return $ toResponse (unlines ([ "ExitCode: " ++ show ec
                                               , "stdout:"
                                               , out
                                               , "stderr:"
                                               , err
                                               , "input:"
                                               ] ++
                                               showLines (rsBody response)))
    | otherwise = return response
    where
      column = "  " ++ (take 120 $ concatMap  (\n -> "         " ++ show n) (drop 1 $ cycle [0..9::Int]))
      showLines :: L.ByteString -> [String]
      showLines string = column : zipWith (\n -> \l  -> show n ++ " " ++ (L.unpack l)) [1::Integer ..] (L.lines string)

-- "Pattern match failure in do expression at src\AppControl.hs:43:24"
-- is converted to:
-- "src\AppControl.hs:43:24: Pattern match failure in do expression"
-- Then we output this to stderr. Help debugging under Emacs console when using GHCi.
-- This is GHC specific, but you may add your favourite compiler here also.
outputTraceMessage s c | "Pattern match failure " `isPrefixOf` s = 
    let w = [(k,p) | (i,p) <- zip (tails s) (inits s), Just k <- [stripPrefix " at " i]]
        v = concatMap (\(k,p) -> k ++ ": " ++ p) w
    in trace v c
outputTraceMessage _ c = trace "some error" c


mkFailMessage :: (FilterMonad Response m, WebMonad Response m) => String -> m b
mkFailMessage s = do
    ignoreFilters
    internalServerError ()
    setHeaderM "Content-Type" "text/html"
    res <- return $ toResponse $ failHtml s
    finishWith $ res

failHtml:: String->String
failHtml errString = 
   "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">"
    ++ "<html><head><title>Happstack "
    ++ ver ++ " Internal Server Error</title></head>"
    ++ "<body><h1>Happstack " ++ ver ++ "</h1>"
    ++ "<p>Something went wrong here<br>"
    ++ "Internal server error<br>"
    ++ "Everything has stopped</p>"
    ++ "<p>The error was \"" ++ (escapeString errString) ++ "\"</p></body></html>"
    where ver = DV.showVersion Cabal.version

escapeString :: String -> String
escapeString str = concatMap encodeEntity str
    where
      encodeEntity :: Char -> String
      encodeEntity '<' = "&lt;"
      encodeEntity '>' = "&gt;"
      encodeEntity '&' = "&amp;"
      encodeEntity '"' = "&quot;"
      encodeEntity c
          | ord c > 127 = "&#" ++ show (ord c) ++ ";"
          | otherwise = [c]

notFoundHtml :: String
notFoundHtml = 
    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">"
    ++ "<html><head><title>Happstack "
    ++ ver ++ " File not found</title></head>"
    ++ "<body><h1>Happstack " ++ ver ++ "</h1>"
    ++ "<p>Your file is not found<br>"
    ++ "To try again is useless<br>"
    ++ "It is just not here</p>"
    ++ "</body></html>"
    where ver = DV.showVersion Cabal.version
