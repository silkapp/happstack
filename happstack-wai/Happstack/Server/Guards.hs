{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables #-}
module Happstack.Server.Guards where

import Control.Monad(MonadPlus(mzero), unless)
import Happstack.Server.Monads
import Network.Wai
import System.FilePath                           (makeRelative, splitDirectories)

-------------------------------------
-- guards

-- | guard using an arbitrary function on the request
guardRq :: (ServerMonad m, MonadPlus m) => (([FilePath], Request) -> Bool) -> m ()
guardRq f = do
    rq <- askRq
    unless (f rq) mzero

-- | Guard against non-empty remaining path segments
nullDir :: (ServerMonad m, MonadPlus m) => m ()
nullDir = guardRq $ \(paths, _rq) -> null paths

-- | Pop a path element and run the @ServerPartT@ if it matches the given string.
-- 
-- The path element can not contain '/'. See also 'dirs'.
dir :: (ServerMonad m, MonadPlus m) => String -> m a -> m a
dir staticPath handle =
    do (paths, rq) <- askRq
       case paths of
         (p:xs) | p == staticPath -> localRq (\(_, newRq) -> (xs, newRq)) handle
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
