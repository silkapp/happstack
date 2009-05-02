module Happstack.State.Saver.Types where

data ReaderStream a
    = ReaderStream
    { readerClose    :: IO ()
    , readerGet      :: IO ([a], Int)
    , readerGetUncut :: IO [a]
    }

data WriterStream a
    = WriterStream
    { writerClose         :: IO ()
    , writerAdd           :: a -> IO () -> IO ()
    , writerAtomicReplace :: a -> IO ()
    , writerCut           :: IO Int
    }

