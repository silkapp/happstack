{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, RecursiveDo #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  HAppS.Util.TimeOut
-- Copyright   :  (c) HAppS.org, 2005
-- License     :  BSD3
-- 
-- Portability :  uses mdo
--
-- Timeout implementation for performing operations in the IO monad
-- with a timeout added. Both using Maybe and exceptions to handle
-- timeouts are supported.
--
-- Timeouts can be implemented in GHC with either a global handler
-- or a per-timeout thread which sleeps until the timeout. The latter
-- is used in this module. Blocking on foreign calls can cause
-- problems as GHC has no way of interrupting such threads.
-- The module provides a slightly slower alternative implementation
-- which returns even if the computation has blocked on a foreign
-- call. This should not be an issue unless -threaded is used.
--
-- The timeouts are currently limited to a maximum of about
-- 2000 seconds. This is a feature of threadDelay, but
-- supporting longer timeouts is certainly possible if
-- that is desirable.
--
-- For nested timeouts there are different ways to implement them:
-- a) attach an id to the exception so that the catch knows wether it may catch
--    this timout exception. I've choosen this because overhead is only passing
--    and incrementing an integer value. A integer wrap araound is possible but
--    too unlikely to happen to make me worry about it
-- b) start a new workiing and killing thread so that if the original thread
--   was run within withTimeOut itself it catches the exception and not an inner
--   timout. (this is done in withSafeTimeOut, for another reason though)
-- c) keep throwing exceptions until the the withTimeOut function kills the
--   killing thread. But consider sequence (forever (timeOut threadDelay 10sec) )
--   In this case the exception will be called and the next timOut may be entered
--   before the second Exception has been thrown
--
-- All exceptions but the internal TimeOutExceptionI are rethrown in the calling thread
-----------------------------------------------------------------------------
module HAppS.Util.TimeOut 
    (withTimeOut, withTimeOutMaybe,
     withSafeTimeOut, withSafeTimeOutMaybe,
     TimeOutException(..), second
    ) where

import Control.Concurrent
import Control.Exception.Extensible as E
import Data.Typeable(Typeable)
import Data.IORef
import Data.Maybe
import System.IO.Unsafe (unsafePerformIO)

import HAppS.Util.Concurrent

type TimeOutTId = Int -- must be distinct within a thread only 

{-# NOINLINE timeOutIdState #-}
timeOutIdState :: IORef TimeOutTId
timeOutIdState = unsafePerformIO $ newIORef minBound

nextTimeOutId :: IO TimeOutTId
nextTimeOutId = do
  atomicModifyIORef timeOutIdState (\a -> let nid =nextId a in (nid,nid))
  where nextId i | i == maxBound = minBound
        nextId i = i + 1

data TimeOutExceptionI = TimeOutExceptionI TimeOutTId -- internal exception, should only be used within this module 
  deriving(Typeable)

data TimeOutException = TimeOutException -- that's the exception the user may catch 
  deriving(Typeable)

instance Show TimeOutExceptionI where show _ = error "this TimeOutExceptionI should have been caught within this module"
instance E.Exception TimeOutExceptionI

deriving instance Show TimeOutException
instance E.Exception TimeOutException

throw' :: Exception exception => exception -> b
throw' = throw

throwTo' :: Exception e => ThreadId -> e -> IO ()
throwTo' = E.throwTo

catch' :: Exception e => IO a -> (e -> IO a) -> IO a
catch' = E.catch

try' :: IO a -> IO (Either SomeException a) -- give a type signature for try 
try' = E.try


-- module internal function 
catchTimeOutI :: TimeOutTId -> IO a -> IO a -> IO a
catchTimeOutI toId op handler =
  op `catch'` (\e@(TimeOutExceptionI i) -> if i == toId then handler  else throw' e)


-- | This is the normal timeout handler. It throws a TimeOutException exception,
-- if the timeout occurs.

withTimeOutMaybe :: Int -> IO a -> IO (Maybe a)
withTimeOutMaybe tout op = do 
  toId <- nextTimeOutId
  wtid <- myThreadId
  ktid <- fork ( do threadDelay tout 
                    throwTo' wtid (TimeOutExceptionI toId)
               )
  (catchTimeOutI toId) (fmap Just (op >>= \r -> killThread ktid >> return  r)) (return Nothing)

withTimeOut :: Int -> IO a -> IO a
withTimeOut tout op = maybeToEx =<< withTimeOutMaybe tout op

maybeToEx :: (Monad m) => Maybe t -> m t  
maybeToEx (Just r) = return r
maybeToEx Nothing = throw' TimeOutException

-- | Like timeOut, but additionally it works even if the computation is blocking
-- async exceptions (explicitely or by a blocking FFI call). This consumes
-- more resources than timeOut, but is still quite fast.
withSafeTimeOut :: Int -> IO a -> IO a
withSafeTimeOut tout op = maybeToEx =<< withSafeTimeOutMaybe tout op

-- | Like withTimeOutMaybe, but handles the operation blocking exceptions like withSafeTimeOut
-- does.
withSafeTimeOutMaybe :: Int -> IO a -> IO (Maybe a)
withSafeTimeOutMaybe tout op = mdo
  mv <- newEmptyMVar
  wt <- fork $ do 
          t <- try' op
          case t of
            Left e -> tryPutMVar mv (Left e)
            Right r -> tryPutMVar mv (Right (Just r))
          killThread kt
  kt <- fork $ do 
          threadDelay tout
          e <- tryPutMVar mv (Right Nothing)
          if e then killThread wt else return ()
  eitherToEx =<< takeMVar mv
  where eitherToEx (Left e) = throw' e
        eitherToEx (Right r) = return r
  

-- | Constant representing one second.
second :: Int
second = 1000000
