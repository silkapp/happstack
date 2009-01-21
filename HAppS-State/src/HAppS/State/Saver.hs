module HAppS.State.Saver
    ( module HAppS.State.Saver.Types
    , Saver(..)
    , createReader, createWriter ) where

import Control.Concurrent
import HAppS.State.Saver.Impl.File
import HAppS.State.Saver.Impl.Memory
import HAppS.State.Saver.Impl.Queue
import HAppS.State.Saver.Types
import HAppS.Data.Serialize

data Saver = NullSaver        -- ^ A saver that discards all output
           | FileSaver String -- ^ A saver that operates on files. The parameter is the prefix for the files.
                              --   Creates the prefix directory.
           | Queue Saver      -- ^ Enable queueing.
           | Memory (MVar Store)

createReader :: Serialize a => Saver -> String -> Int -> IO (ReaderStream a)
createReader (FileSaver prefix) key cutoff = fileReader prefix key cutoff
createReader (Memory store) key cutoff = memoryReader store key cutoff
createReader (Queue saver)      key cutoff = queueReader =<< createReader saver key cutoff
createReader NullSaver _key _cutoff
    = return $ ReaderStream
               { readerClose = return ()
               , readerGet   = fail "NullSaver: readerGet"
               , readerGetUncut = fail "NullSaver: readerGetUncut" }

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



