{-# LANGUAGE DeriveDataTypeable #-}
module HAppS.State.Saver.Impl.Queue
    ( queueReader
    , queueWriter
    ) where

import HAppS.State.Saver.Types
import HAppS.Data.Serialize

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Data.Typeable

import Data.Binary

data Item = Close (IO ())
          | Add Put (IO ())

queueReader :: ReaderStream a -> IO (ReaderStream a)
queueReader stream = return stream

data Encoded = Encoded Put deriving Typeable
instance Version Encoded where mode = Primitive
instance Serialize Encoded where
    putCopy (Encoded out) = contain out
    getCopy = error "decoding from writer queue."

-- | A saver that bunches writes.
queueWriter :: Serialize a => WriterStream Encoded -> IO (WriterStream a)
queueWriter writer = do
  ch   <- newCh
  let handler = do
        input <- getChs ch
        let il (Add p f : rest) a0 a1 = il rest (p:a0) (f:a1)
            il something        a0 a1 = (reverse a0, sequence_ $ reverse a1, something)
        let (ps,io,rest) = il input [] []
        when (not (null ps)) $ writerAdd writer (Encoded $ sequence_ ps) io
        case rest of
          []           -> handler
          (Close io':_) -> writerClose writer >> io'
          _            -> fail "queueSaver: Invalid saver bunch!"

  forkIO handler
  return $ WriterStream
    { writerClose = do
        mv <- newEmptyMVar
        writeCh ch $ Close (writerClose writer >> putMVar mv ())
        takeMVar mv
    , writerAdd   = \ps fin -> writeCh ch $ Add (safePut ps) fin
    , writerAtomicReplace = \a -> writerAtomicReplace writer (Encoded $ safePut a)
    , writerCut = writerCut writer
    }


-- Sample variables/queues
newtype Ch a = Ch (TVar [a])
newCh :: IO (Ch a)
newCh = fmap Ch $ newTVarIO []
writeCh :: Ch a -> a -> IO ()
writeCh (Ch ch) x = atomically $ do vs <- readTVar ch
                                    writeTVar ch (x:vs)

getChs :: Ch a -> IO [a]
getChs (Ch ch) = atomically $ do vs <- readTVar ch
                                 guard (not (null vs))
                                 writeTVar ch []
                                 return (reverse vs)


