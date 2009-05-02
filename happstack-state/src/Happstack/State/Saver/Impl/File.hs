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
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as B
import System.Directory         ( createDirectoryIfMissing, removeFile, renameFile, doesFileExist )
import System.IO
import qualified System.IO.Error as SE
import System.Random            ( randomIO )
import System.Log.Logger
import Text.Printf
import Control.Monad
import System.FilePath
import Happstack.Util.OpenExclusively (openExclusively)

type PrefixLock = (FilePath, Handle)

tryE :: IO a -> IO (Either SomeException a)
tryE = E.try

catchE :: IO a -> (SomeException -> IO a) -> IO a
catchE = E.catch

catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO = E.catch

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


