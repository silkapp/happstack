{-# LANGUAGE CPP, ScopedTypeVariables, PatternSignatures #-}
module HAppS.Server.HTTP.Listen(listen) where

import System.Log.Logger

import HAppS.Server.HTTP.Types
import HAppS.Server.HTTP.Handler

import Control.Exception.Extensible as E
import Control.Concurrent
import Network
import System.IO

{-
#ifndef mingw32_HOST_OS
-}
import System.Posix.Signals
{-
#endif
-}

listen :: Conf -> (Request -> IO Response) -> IO ()
listen conf hand = do
{-
#ifndef mingw32_HOST_OS
-}
  installHandler openEndedPipe Ignore Nothing
{-
#endif
-}
  s <- listenOn $ PortNumber $ toEnum $ port conf
  let work (h,hn,p) = do -- hSetBuffering h NoBuffering
                         let eh (x::SomeException) = logM "HAppS.Server.HTTP.Listen" ERROR ("HTTP request failed with: "++show x)
                         request conf h (hn,fromIntegral p) hand `E.catch` eh
                         hClose h
  let msg = "\nIPV6 is not supported yet. \nLikely you made a localhost request \n"++
            "and your machine resolved localhost to an IPv6 address. \n"++
            "Use http://127.0.0.1:"++(show $ port conf)++"\n"
      loop = do accept s >>= forkIO . work
                loop
  --  let loop = accept s >>= forkIO . work >> loop
  let pe e = logM "HAppS.Server.HTTP.Listen" ERROR ("ERROR in accept thread: "++
                                                    show e)
  let infi = loop `catchSome` pe >> infi -- loop `E.catch` pe >> infi
  infi `finally` sClose s
{--
#ifndef mingw32_HOST_OS
-}
  installHandler openEndedPipe Ignore Nothing
  return ()
{-
#endif
-}
  where  -- why are these handlers needed?

    -- catchSome op h :: IO () -> (SomeException -> IO () ) -> IO ()
    catchSome op h = op `E.catches` [
            Handler $ \(e :: ArithException) -> h (toException e),
            Handler $ \(e :: ArrayException) -> h (toException e)
          ]
