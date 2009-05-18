-------------------------------------------------------------------------------
-- |
-- Module      :  Happstack.Util.AutoBuild
-- Copyright   :  Happstack.com 2009
-- License     :  BSD3
--
-- Maintainer  :  Matthew Elder
-- Stability   :  provisional
-- Portability :  linux/windows
--
-------------------------------------------------------------------------------
module Happstack.Util.AutoBuild (
    autoBuild
    ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket)
import System.Directory (getModificationTime)
import System.Exit (ExitCode(..), exitFailure)
import System.Process
import System.Time (ClockTime)
import System.IO

-- | Functionality for the autoBuild tool.
--   Inspired by searchpath.
autoBuild :: String   -- ^ Build command
          -> String   -- ^ Path to binary
          -> [String] -- ^ Arguments to use when running binary
          -> IO ()
autoBuild buildCmd binPath binArgs = do
    putStrLn "Performing the initial build. . ."
    buildSuccess <- buildBin buildCmd -- initial build
    if buildSuccess
        then do
            mph <- newEmptyMVar
            newMod <- getModificationTime binPath
            forkIO (builder mph buildCmd binPath newMod)
            runner mph binPath binArgs
            
        else do
            putStrLn "Initial build failed, see 'build.out.log' and 'build.err.log'."
            exitFailure

-- builds the program
builder :: MVar ProcessHandle -> String -> FilePath -> ClockTime -> IO ()
builder mph buildCmd binPath lastMod = do
    -- add a delay between build attempts (5 seconds)
    threadDelay 5000000
    
    buildSuccess <- buildBin buildCmd
    newMod <- getModificationTime binPath
    
    -- if the build yielded a new binary, terminate the process
    if buildSuccess && (newMod /= lastMod)
        then do
            putStrLn "A new binary has been built, killing the existing one. . ."
            terminateProcess =<< takeMVar mph
        else return ()
        
    -- continue loop
    builder mph buildCmd binPath newMod

-- runs the program
runner :: MVar ProcessHandle -> FilePath -> [String] -> IO ()
runner mph binPath binArgs = do
    bracket
        (runBin binPath binArgs)
        (terminateProcess)
        (\ph -> putMVar mph ph >> waitForProcess ph)
    
    -- continue loop
    runner mph binPath binArgs

-- does not block, returns ph
runBin :: String -> [String] -> IO ProcessHandle
runBin binPath binArgs = do
    putStrLn $ "Running binary: " ++ (showCmd binPath binArgs)
    ph <- runProcess binPath binArgs Nothing Nothing Nothing Nothing Nothing
    return ph
    where showCmd bp [] = bp
          showCmd bp ba = bp ++ " " ++ unwords ba

-- blocks until built, returns True if build was a success
buildBin :: String -> IO Bool
buildBin buildCmd = do
    (_inp,out,err,ph) <- runInteractiveCommand buildCmd
    appendFile "build.out.log" =<< hGetContents out
    appendFile "build.err.log" =<< hGetContents err
    waitForProcess ph
    exitCode <- getProcessExitCode ph
    return (exitCode == Just ExitSuccess)

