{-# LANGUAGE FlexibleContexts #-}
-- | This module provides three classes and some related functions
-- which provide 'ServerPartT' with much of its web-centric behavior.
--
--  1. 'ServerMonad' provides access to the HTTP 'Request'
--
--  2. 'FilterMonad' provides the ability to apply filters and transformations to a 'Response'
--
--  3. 'WebMonad' provides a way to escape a computation and return a 'Response'
module Happstack.Server.Monads
    ( -- * ServerPartT
      ServerPartT
    , ServerPart
      -- * ServerMonad
    , ServerMonad(..)
    , mapServerPartT
    , mapServerPartT'
    , UnWebT
    , filterFun
      -- * FilterMonad
    , FilterMonad(..)
    , ignoreFilters
    , addHeaderM
    , getHeaderM
    , setHeaderM
      -- * WebMonad
    , WebMonad(..)
    , escape
    , escape' 
      -- * MonadPlus helpers
    , require
    , requireM
    ) where

import Control.Monad (MonadPlus(mzero))
import Control.Monad.Trans (MonadIO(..),MonadTrans(lift))
import qualified Data.ByteString.Char8           as B
import Happstack.Server.Internal.Monads
import Happstack.Server.Types (Response, addHeader, getHeader, setHeader)

-- | Used to ignore all your filters and immediately end the
-- computation.  A combination of 'ignoreFilters' and 'finishWith'.
escape :: (WebMonad a m, FilterMonad a m) => m a -> m b
escape gen = ignoreFilters >> gen >>= finishWith

-- | An alternate form of 'escape' that can be easily used within a do
-- block.
escape' :: (WebMonad a m, FilterMonad a m) => a -> m b
escape' a = ignoreFilters >> finishWith a

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


