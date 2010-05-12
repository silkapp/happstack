{-# LANGUAGE FlexibleContexts, FlexibleInstances, RankNTypes, ScopedTypeVariables #-}
module Happstack.Server.Guards where

import Control.Monad(MonadPlus(mzero), unless)
import Happstack.Server.Monads
import Network.Wai hiding (Request)
import System.FilePath                           (makeRelative, splitDirectories)

-------------------------------------
-- guards

-- | guard using an arbitrary function on the request
guardRq :: (ServerMonad m, MonadPlus m) => (Request -> Bool) -> m ()
guardRq f = do
    rq <- askRq
    unless (f rq) mzero

-- | Guard against non-empty remaining path segments
nullDir :: (ServerMonad m, MonadPlus m) => m ()
nullDir = guardRq $ null . rqPaths

-- | Pop a path element and run the @ServerPartT@ if it matches the given string.
-- 
-- The path element can not contain '/'. See also 'dirs'.
dir :: (ServerMonad m, MonadPlus m) => String -> m a -> m a
dir staticPath handle =
    do rq <- askRq
       case rqPaths rq of
         (p:xs) | p == staticPath -> localRq (\newRq -> newRq {rqPaths = xs}) handle
         _ -> mzero

-- | guard against a 'FilePath'. Unlike 'dir' the 'FilePath' may
-- contain '/'. If the guard succeeds, the matched elements will be
-- popped from the directory stack.
--
-- > dirs "foo/bar" $ ...
--          
-- see also: 'dir'
dirs :: (ServerMonad m, MonadPlus m) => FilePath -> m a -> m a 
dirs fp m = 
     do let parts = splitDirectories (makeRelative "/" fp) 
        foldr dir m parts


class MatchMethod m where matchMethod :: m -> Method -> Bool
instance MatchMethod Method where matchMethod m = (== m)
instance MatchMethod [Method] where matchMethod methods = (`elem` methods)
instance MatchMethod (Method -> Bool) where matchMethod f = f
instance MatchMethod () where matchMethod () _ = True

-- | Guard against the method.  This function also guards against
-- any remaining path segments.  See methodOnly for the version
-- that guards only by method
methodM :: (ServerMonad m, MonadPlus m, MatchMethod method) => method -> m ()
methodM meth = methodOnly meth >> nullDir

-- | guard against the method only. (as opposed to 'methodM')
methodOnly :: (ServerMonad m, MonadPlus m, MatchMethod method) => method -> m ()
methodOnly meth = guardRq $ matchMethod meth . requestMethod . rqRequest

-- | Guard against the method. Note, this function also guards against any
--   remaining path segments.
methodSP :: (ServerMonad m, MonadPlus m, MatchMethod method) => method -> m b-> m b
methodSP m handle = methodM m >> handle