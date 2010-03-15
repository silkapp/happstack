{-# LANGUAGE FunctionalDependencies, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeSynonymInstances, RankNTypes, ScopedTypeVariables #-}
module Happstack.Server.Monads where
       
import Control.Monad (unless)
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.UTF8       as LU (toString, fromString)
import Data.Monoid
import Control.Monad (MonadPlus(mzero), msum)
import Control.Monad.Reader(ReaderT(..), MonadReader(ask, local))
import Control.Monad.Trans(MonadIO(..))
import Control.Monad.Error(ErrorT(..), Error(strMsg), throwError)
import Control.Monad.Maybe                       (MaybeT(MaybeT), runMaybeT)
import Control.Monad.Writer
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.UTF8       as LU (toString, fromString)
import Network.Wai hiding (Request)
import qualified Network.Wai as Wai
import Network.Wai.Enumerator (fromLBS)

instance Error Response where
  strMsg str = 
    Response { status = Status200
             , responseHeaders = [(ContentType, B.pack "text/plain; charset=UTF-8")]
             , responseBody = Right (fromLBS (LU.fromString str))
             }

data Request
    = Request
      { rqRequest :: Wai.Request
      , rqPaths   :: [FilePath]
      }

-- The monads:

-- FilterT

-- | A monoid operation container.
-- If a is a monoid, then SetAppend is a monoid with the following behaviors:
--
-- >  Set    x `mappend` Append y = Set    (x `mappend` y)
-- >  Append x `mappend` Append y = Append (x `mappend` y)
-- >  _        `mappend` Set y    = Set y
--
-- A simple way of summarizing this is, if the right side is Append, then the
-- right is appended to the left.  If the right side is Set, then the left side
-- is ignored.

data SetAppend a = Set a | Append a
    deriving (Eq, Show)

instance Monoid a => Monoid (SetAppend a) where
   mempty = Append mempty

   Set    x `mappend` Append y = Set    (x `mappend` y)
   Append x `mappend` Append y = Append (x `mappend` y)
   _        `mappend` Set y    = Set y

-- | Extract the value from a SetAppend
-- Note that a SetAppend is actually a CoPointed from:
-- <http://hackage.haskell.org/packages/archive/category-extras/latest/doc/html/Control-Functor-Pointed.html>
-- But lets not drag in that dependency. yet...
extract :: SetAppend t -> t
extract (Set    x) = x
extract (Append x) = x

instance Functor (SetAppend) where
    fmap f (Set    x) = Set    $ f x
    fmap f (Append x) = Append $ f x

-- | @FilterFun@ is a lot more fun to type than @SetAppend (Dual (Endo a))@
type FilterFun a = SetAppend (Dual (Endo a))

unFilterFun :: FilterFun a -> (a -> a)
unFilterFun = appEndo . getDual . extract

newtype FilterT a m b = FilterT { unFilterT :: WriterT (FilterFun a) m b }
   deriving (Monad, MonadTrans, Functor, MonadIO)

-- | A set of functions for manipulating filters.  A ServerPartT implements
-- FilterMonad Response so these methods are the fundamental ways of
-- manipulating the response object, especially before you've converted your
-- monadic value to a 'Response'
class Monad m => FilterMonad a m | m->a where
    -- | Ignores all previous
    -- alterations to your filter
    --
    -- As an example:
    --
    -- > do
    -- >   composeFilter f
    -- >   setFilter g
    -- >   return "Hello World"
    --
    -- setFilter g will cause the first composeFilter to be
    -- ignored.
    setFilter :: (a->a) -> m ()
    -- |
    -- composes your filter function with the
    -- existing filter function.
    composeFilter :: (a->a) -> m ()
    -- | retrives the filter from the environment
    getFilter :: m b -> m (b, a->a)

instance (Monad m) => FilterMonad a (FilterT a m) where
    setFilter     = FilterT . tell                . Set    . Dual . Endo
    composeFilter = FilterT . tell                . Append . Dual . Endo
    getFilter     = FilterT . listens unFilterFun . unFilterT
    
    
-- WebMonad
    
class Monad m => WebMonad a m | m->a where
    -- | A control structure
    -- It ends the computation and returns the Response you passed into it
    -- immediately.  This provides an alternate escape route.  In particular
    -- it has a monadic value of any type.  And unless you call @'setFilter' id@
    -- first your response filters will be applied normally.
    --
    -- Extremely useful when you're deep inside a monad and decide that you
    -- want to return a completely different content type, since it doesn't
    -- force you to convert all your return types to Response early just to
    -- accomodate this.
    finishWith :: a -> m b

-- WebT

-- | The basic response building object.
newtype WebT m a = WebT { unWebT :: ErrorT Response (FilterT Response (MaybeT m)) a }
    deriving (MonadIO, Functor)
             
type UnWebT m a = m (Maybe (Either Response a, FilterFun Response))
             
instance (Monad m) => Monad (WebT m) where
    m >>= f = WebT $ unWebT m >>= unWebT . f
    return a = WebT $ return a
    fail s = undefined -- mkFailMessage s
    
instance (Monad m) => MonadPlus (WebT m) where
    -- | Aborts a computation.
    --
    -- This is primarily useful because msum will take an array
    -- of MonadPlus and return the first one that isn't mzero,
    -- which is exactly the semantics expected from objects
    -- that take lists of ServerPartT
    mzero = WebT $ lift $ lift $ mzero
    mplus x y =  WebT $ ErrorT $ FilterT $ (lower x) `mplus` (lower y)
        where lower = (unFilterT . runErrorT . unWebT)
              
instance (Monad m) => FilterMonad Response (WebT m) where
    setFilter f = WebT $ lift $ setFilter $ f
    composeFilter f = WebT . lift . composeFilter $ f
    getFilter     m = WebT $ ErrorT $ fmap lft $ getFilter (runErrorT $ unWebT m)
        where
          lft (Left  r, _) = Left r
          lft (Right a, f) = Right (a, f)

instance (Monad m) => WebMonad Response (WebT m) where
    finishWith r = WebT $ throwError r

{-    
-- | takes your WebT, if it is 'mempty' it returns Nothing else it
-- converts the value to a Response and applies your filter to it.
runWebT :: forall m b. (Functor m, ToMessage b) => WebT m b -> m (Maybe Response)
runWebT = (fmap . fmap) appFilterToResp . ununWebT
    where
      appFilterToResp :: (Either Response b, FilterFun Response) -> Response
      appFilterToResp (e, ff) = unFilterFun ff $ either id toResponse e
-}

-- | for when you really need to unpack a WebT entirely (and not
-- just unwrap the first layer with unWebT)
ununWebT :: WebT m a -> UnWebT m a
ununWebT = runMaybeT . runWriterT . unFilterT . runErrorT . unWebT

-- converts the value to a Response and applies your filter to it.
runWebT :: forall res m b. (Functor m) => WebT m Response -> m (Maybe Response)
runWebT = (fmap . fmap) appFilterToResp . ununWebT
    where
      appFilterToResp :: (Either Response Response, FilterFun Response) -> Response
      appFilterToResp (e, ff) = unFilterFun ff $ either id id e

mkFailMessage :: (FilterMonad res m, WebMonad res m) => String -> m b
mkFailMessage s = undefined {- do
    ignoreFilters
    internalServerError ()
    setHeaderM "Content-Type" "text/html"
    res <- return $ toResponse $ failHtml s
    finishWith $ res
    -}


-- ServerMonad                   

class Monad m => ServerMonad m  where
    askRq   :: m Request
    localRq :: (Request -> Request) -> m a -> m a

-- | ServerPartT is a container for processing requests and returning results
newtype ServerPartT m a = ServerPartT { unServerPartT :: ReaderT Request (WebT m) a }
    deriving (Monad, MonadIO, MonadPlus, Functor)

instance (Monad m) => ServerMonad (ServerPartT m) where
    askRq = ServerPartT $ ask
    localRq f m = ServerPartT $ local f (unServerPartT m)

runServerPartT :: ServerPartT m a -> Request -> WebT m a
runServerPartT = runReaderT . unServerPartT
