{-# LANGUAGE CPP #-}
module Happstack.State.Saver.Impl.File
    ( PrefixLock
    , fileReader
    , fileWriter
    , obtainPrefixLock
    , releasePrefixLock
    ) where

import Happstack.State.Saver.Types
import Happstack.Data.Serialize

import Control.Concurrent
import Control.Exception.Extensible as E
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import System.Directory         ( createDirectoryIfMissing, removeFile, renameFile, doesFileExist )
import System.IO
import System.Random            ( randomIO )
import System.Log.Logger
import Text.Printf
import Control.Monad
import System.FilePath
#ifdef UNIX
import Data.Maybe (listToMaybe)
import qualified System.IO.Error as SE
import System.Posix.IO (openFd, OpenMode(ReadWrite), defaultFileFlags, exclusive, trunc, fdToHandle)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (nullSignal, signalProcess)
import System.Posix.Types (ProcessID)
#else
import Happstack.Util.OpenExclusively (openExclusively)
#endif

#ifdef UNIX
newtype PrefixLock = PrefixLock FilePath
#else
type PrefixLock = (FilePath, Handle)
#endif

tryE :: IO a -> IO (Either SomeException a)
tryE = E.try

catchE :: IO a -> (SomeException -> IO a) -> IO a
catchE = E.catch

#ifndef UNIX
catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO = E.catch
#endif

logMF :: Priority -> String -> IO ()
logMF = logM "Happstack.State.Saver.Impl.File"

formatFilePath :: Int -> String -> FilePath
formatFilePath n str = printf "%s-%010d" str n

fileReader :: Serialize a => FilePath -> String -> Int -> IO (ReaderStream a)
fileReader prefix key cutoff
    = do let file = prefix </> formatFilePath cutoff key
         tryE $ createDirectoryIfMissing True prefix
         return $ ReaderStream
                    { readerClose = return ()
                    , readerGet   = do logMF NOTICE "fileReader: readerGet"
                                       allFiles <- getAllFiles prefix key cutoff
                                       allData <- mapM B.readFile allFiles
                                       return $ (parseAll (L.fromChunks allData), length allFiles)
                    , readerGetUncut = do logMF NOTICE "fileReader: readerGetUncut"
                                          allData <- B.readFile file `catchE` \_ -> return B.empty
                                          return $ parseAll (L.fromChunks [allData])
                    }


parseAll :: Serialize a => L.ByteString -> [a]
parseAll = loop
    where loop l | L.null l = []
          loop l = let (a,rest) = deserialize l
                   in a:loop rest

fileWriter :: Serialize a => FilePath -> String -> Int -> IO (WriterStream a)
fileWriter prefix key cutoffInit = do
  cutoffVar <- newMVar cutoffInit
  let getFileName = do cutoff <- readMVar cutoffVar
                       return $ prefix </> formatFilePath cutoff key
  file <- getFileName
  logMF NOTICE ("fileWriter: "++key++" @ "++prefix)
  hmv <- newMVar =<< openBinaryFile file WriteMode
  return $ WriterStream
             { writerClose = withMVar hmv hClose
             , writerAdd   = \m f -> do logMF NOTICE "fileWriter: saverAdd"
                                        withMVar hmv (\h -> L.hPut h (serialize m) >> hFlush h)
                                        forkIO f
                                        return ()
             , writerAtomicReplace = \ss -> do h <- takeMVar hmv
                                               hClose h
                                               file' <- getFileName
                                               atomicWriteFile file' (serialize ss)
                                               putMVar hmv =<< openBinaryFile file' AppendMode
             , writerCut = do h <- takeMVar hmv
                              hClose h
                              cutoff <- takeMVar cutoffVar
                              let file' = prefix </> formatFilePath (cutoff+1) key
                              putMVar cutoffVar (cutoff+1)
                              putMVar hmv =<< openBinaryFile file' WriteMode
                              return (cutoff+1)
                 }

getAllFiles :: FilePath -> String -> Int -> IO [FilePath]
getAllFiles prefix key cutoff
    = loop cutoff
    where loop n = do let file = prefix </> formatFilePath n key
                      exist <- doesFileExist file
                      if exist then liftM (file:) (loop (n+1))
                               else return []

-- | Just to avoid a dependency.
atomicWriteFile :: String -> L.ByteString -> IO ()
atomicWriteFile path string = do
  r <- randomIO :: IO Int
  let p' = path ++ ".atomic-tmp-" ++ show (abs r)
  L.writeFile p' string
  renameFile p' path

#ifdef UNIX
obtainPrefixLock :: FilePath -> IO PrefixLock
obtainPrefixLock prefix = do
    checkLock fp >> takeLock fp
    where fp = prefix ++ ".lock"

-- |Read the lock and break it if the process is dead.
checkLock :: FilePath -> IO ()
checkLock fp = readLock fp >>= maybeBreakLock fp

-- |Read the lock and return the process id if possible.
readLock :: FilePath -> IO (Maybe ProcessID)
readLock fp = try (readFile fp) >>=
              return . either (checkReadFileError fp) (fmap (fromInteger . read) . listToMaybe . lines)

-- |Is this a permission error?  If so we don't have permission to
-- remove the lock file, abort.
checkReadFileError :: [Char] -> IOError -> Maybe ProcessID
checkReadFileError fp e | SE.isPermissionError e = throw (userError ("Could not read lock file: " ++ show fp))
                        | SE.isDoesNotExistError e = Nothing
                        | True = throw e

maybeBreakLock :: FilePath -> Maybe ProcessID -> IO ()
maybeBreakLock fp Nothing =
    -- The lock file exists, but there's no PID in it.  At this point,
    -- we will break the lock, because the other process either died
    -- or will give up when it failed to read its pid back from this
    -- file.
    breakLock fp
maybeBreakLock fp (Just pid) = do
  -- The lock file exists and there is a PID in it.  We can break the
  -- lock if that process has died.
  -- getProcessStatus only works on the children of the calling process.
  -- exists <- try (getProcessStatus False True pid) >>= either checkException (return . isJust)
  exists <- doesProcessExist pid
  case exists of
    True -> throw (lockedBy fp pid)
    False -> breakLock fp

doesProcessExist :: ProcessID -> IO Bool
doesProcessExist pid =
    -- Implementation 1
    -- doesDirectoryExist ("/proc/" ++ show pid)
    -- Implementation 2
    try (signalProcess nullSignal pid) >>= return . either checkException (const True)
    where checkException e | SE.isDoesNotExistError e = False
                           | True = throw e

-- |We have determined the locking process is gone, try to remove the
-- lock.
breakLock :: FilePath -> IO ()
breakLock fp = try (removeFile fp) >>= either checkBreakError (const (return ()))

-- |An exception when we tried to break a lock, if it says the lock
-- file has already disappeared we are still good to go.
checkBreakError :: IOError -> IO ()
checkBreakError e | SE.isDoesNotExistError e = return ()
                  | True = throw e

-- |Try to create lock by opening the file with the O_EXCL flag and
-- writing our PID into it.  Verify by reading the pid back out and
-- matching, maybe some other process slipped in before we were done
-- and broke our lock.
takeLock :: FilePath -> IO PrefixLock
takeLock fp = do
  createDirectoryIfMissing True (takeDirectory fp) 
  h <- openFd fp ReadWrite (Just 0o600) (defaultFileFlags {exclusive = True, trunc = True}) >>= fdToHandle
  pid <- getProcessID
  hPutStrLn h (show pid) >> hClose h
  -- Read back our own lock and make sure its still ours
  readLock fp >>= maybe (throw (cantLock fp pid))
                        (\ pid' -> if pid /= pid'
                                   then throw (stolenLock fp pid pid')
                                   else return (PrefixLock fp))

-- |An exception saying the data is locked by another process.
lockedBy :: (Show a) => FilePath -> a -> SomeException
lockedBy fp pid = SomeException (SE.mkIOError SE.alreadyInUseErrorType ("Locked by " ++ show pid) Nothing (Just fp))

-- |An exception saying we don't have permission to create lock.
cantLock :: FilePath -> ProcessID -> SomeException
cantLock fp pid = SomeException (SE.mkIOError SE.alreadyInUseErrorType ("Process " ++ show pid ++ " could not create a lock") Nothing (Just fp))

-- |An exception saying another process broke our lock before we
-- finished creating it.
stolenLock :: FilePath -> ProcessID -> ProcessID -> SomeException
stolenLock fp pid pid' = SomeException (SE.mkIOError SE.alreadyInUseErrorType ("Process " ++ show pid ++ "'s lock was stolen by process " ++ show pid') Nothing (Just fp))

-- |Relinquish the lock by removing it and then verifying the removal.
releasePrefixLock :: PrefixLock -> IO ()
releasePrefixLock (PrefixLock fp) =
    dropLock >>= either checkDrop return
    where
      dropLock = try (removeFile fp)
      checkDrop e | SE.isDoesNotExistError e = return ()
                  | True = throw e
#else
obtainPrefixLock :: FilePath -> IO PrefixLock
obtainPrefixLock prefix = do
    createDirectoryIfMissing True prefix
    -- catchIO obtainLock onError
    catchIO obtainLock onError
    where fp = prefix ++ ".lock" 
          obtainLock = do
              h <- openExclusively fp
              return (fp, h)
          onError e = do
              putStrLn "There may already be an instance of this application running, which could result in a loss of data."
              putStrLn ("Please make sure there is no other application attempting to access '" ++ prefix ++ "'")
              throw e

releasePrefixLock :: PrefixLock -> IO ()
releasePrefixLock (fp, h) = do
     tryE $ hClose h
     tryE $ removeFile fp
     return ()
#endif
