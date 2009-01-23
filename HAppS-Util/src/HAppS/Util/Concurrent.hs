{-# OPTIONS -cpp #-}
{- Copyright (C) 2005 HAppS.org. All Rights Reserved.

   Using HAppS in GHCi

   Because there are many threads and reloading everything
   is slow here is a way to kill all threads from GHCi:
   add -DINTERACTIVE to the command line and use the
   function happsKill from the prompt.
-}
module HAppS.Util.Concurrent where

import Control.Concurrent
import Prelude hiding (catch)
import Control.Exception -- hiding (catch)
#ifdef INTERACTIVE
import System.IO.Unsafe
import System.Mem
#endif

--generic utils
forkEverSt f state = fork (foreverSt f state)
foreverSt f state=do {newState<-f state;foreverSt f newState;}
forkEver a = fork (forever a)

writeChanRight chan x= writeChan chan (Right x)
writeChanLeft chan x= writeChan chan (Left x)

fork_ :: IO a -> IO ()
fork_ c = fork c >> return ()

-- | Fork a new thread.
fork :: IO a -> IO ThreadId

-- | Register an action to be run when ghci is restarted.
registerResetAction :: IO () -> IO ()
-- | Reset state
reset :: IO ()

#ifndef INTERACTIVE
forever a = do {finally a (forever a)}
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
           logM "HAppS.Util.Concurrent" INFO "reset ok"
{-# NOINLINE happsThreadList #-}
happsThreadList = unsafePerformIO $ newMVar []
#endif

-- | Sleep N seconds
sleep :: Int -> IO ()
sleep n = threadDelay (n * second) where second = 1000000
