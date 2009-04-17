module Happstack.State.Saver
    ( module Happstack.State.Saver.Types
    , Saver(..)
    , createReader, createWriter, lock, unlock ) where

import Control.Concurrent
import Happstack.State.Saver.Impl.File
import Happstack.State.Saver.Impl.Memory
import Happstack.State.Saver.Impl.Queue
import Happstack.State.Saver.Types
import Happstack.Data.Serialize

data Saver = NullSaver        -- ^ A saver that discards all output
           | FileSaver String -- ^ A saver that operates on files. The parameter is the prefix for the files.
                              --   Creates the prefix directory.
           | Queue Saver      -- ^ Enable queueing.
           | Memory (MVar Store)

-- | Dispatches over the Saver type provided to return a 'ReaderStream' for the inferred
-- type. 
createReader :: Serialize a => Saver -> String -> Int -> IO (ReaderStream a)
createReader (FileSaver prefix) key cutoff = fileReader prefix key cutoff
createReader (Memory store) key cutoff = memoryReader store key cutoff
createReader (Queue saver)      key cutoff = queueReader =<< createReader saver key cutoff
createReader NullSaver _key _cutoff
    = return $ ReaderStream
               { readerClose = return ()
               , readerGet   = fail "NullSaver: readerGet"
               , readerGetUncut = fail "NullSaver: readerGetUncut" }

-- | Dispatches over the Saver type provided to return a WriterStream for the
-- inferred type. 
createWriter :: Serialize a => Saver -> String -> Int -> IO (WriterStream a)
createWriter (FileSaver prefix) key cutoff = fileWriter prefix key cutoff
createWriter (Memory store) key cutoff = memoryWriter store key cutoff
createWriter (Queue saver)      key cutoff = queueWriter =<< createWriter saver key cutoff
createWriter NullSaver _key _cutoff
    = return $ WriterStream
               { writerClose = return ()
               , writerAdd   = \_ io -> io
               , writerAtomicReplace = fail "NullSaver: writerAtomicReplace"
               , writerCut   = fail "NullSaver: writerCut" }

-- | Returns True if the lock was obtained correctly
lock :: Saver -> IO Bool
lock (FileSaver prefix) = fileLocker prefix
lock (Queue saver) = lock saver
lock _ = return True

-- | Reverses the effects of 'lock'
unlock :: Saver -> IO ()
unlock (FileSaver prefix) = fileUnlocker prefix
unlock (Queue saver) = unlock saver
unlock _ = return ()

