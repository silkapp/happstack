{-# OPTIONS -cpp #-}
{- Copyright (c) Happstack.com, 2009; (c) HAppS.org, 2005

   Using Happstack in GHCi

   Because there are many threads and reloading everything
   is slow here is a way to kill all threads from GHCi:
   add -DINTERACTIVE to the command line and use the
   function happsKill from the prompt.
-}
module Happstack.Util.Concurrent where

import Control.Concurrent
import Prelude hiding (catch)
import Control.Exception -- hiding (catch)
#ifdef INTERACTIVE
import System.IO.Unsafe
import System.Mem
#endif

--generic utils
-- | Equivalent to a composition of fork and foreverSt
forkEverSt :: (t -> IO t) -> t -> IO ThreadId
forkEverSt f = fork . foreverSt f

-- | Similar to forever but with an explicit state parameter threaded through
-- the computation.
foreverSt :: (Monad m) => (t -> m t) -> t -> m b
foreverSt f state= f state >>= foreverSt f

-- | Equivalent to a composition of fork and forever
forkEver :: IO a -> IO ThreadId
forkEver = fork . forever

-- | Lifts the argument with Right before writing it into the chan
writeChanRight :: Chan (Either a b) -> b -> IO ()
writeChanRight chan = writeChan chan . Right

-- | Lifts the argument with Left before writing it into the chan
writeChanLeft :: Chan (Either a b) -> a -> IO ()
writeChanLeft chan = writeChan chan . Left

-- | Fork that throws away the ThreadId
fork_ :: IO a -> IO ()
fork_ c = fork c >> return ()

-- | Fork a new thread.
fork :: IO a -> IO ThreadId

-- | Register an action to be run when ghci is restarted.
registerResetAction :: IO () -> IO ()
-- | Reset state
reset :: IO ()

-- | A version of forever that will gracefully catch IO exceptions and continue
-- executing the provided action.
forever :: IO a -> IO a

#ifndef INTERACTIVE
forever a = finally a (forever a)
fork c = forkIO (c >> return ())
registerResetAction _ = return ()
reset = return ()
#else
forever a = try a >>= w
    where w (Right _)                            = forever a
          w (Left (AsyncException ThreadKilled)) = return ()
          w (Left e)                             = print e >> forever a
registerResetAction x = modifyMVar_ happsThreadList (return . (x:))

fork c = do
    x <- forkIO (c >> return ())
    modifyMVar_ happsThreadList (\xs -> return (killThread x:xs))
    return x
reset = do xs <- swapMVar happsThreadList []
           sequence_ xs
           threadDelay 10000
           performGC
           logM "Happstack.Util.Concurrent" INFO "reset ok"
{-# NOINLINE happsThreadList #-}
happsThreadList = unsafePerformIO $ newMVar []
#endif

-- | Sleep N seconds
sleep :: Int -> IO ()
sleep n = threadDelay (n * second) where second = 1000000
