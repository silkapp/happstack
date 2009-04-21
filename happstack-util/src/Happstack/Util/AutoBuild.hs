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

import Control.Timeout (addTimeout)
import Data.Maybe (isNothing)
import System.Directory (getModificationTime)
import System.Exit (ExitCode(..))
import System.Process
import System.Time (ClockTime)
import System.IO

log' :: [Char] -> IO ()
log' ""  = return ()
log' msg = appendFile "build.log" (msg ++ "\n")

-- | Functionality for the autoBuild tool.
--   Inspired by searchpath.
autoBuild :: String   -- ^ Build command
          -> String   -- ^ Path to binary
          -> [String] -- ^ Arguments to use when running binary
          -> IO ()
autoBuild buildCmd binPath binArgs = do
    log' "Initial Build . . ."
    buildBin buildCmd
    spawnLoop buildCmd binPath binArgs
    return ()

{- flow control -}

-- spawn the binary over and over again
spawnLoop :: String -> String -> [String] -> IO ()
spawnLoop buildCmd binPath binArgs = do
    lastMod <- getModificationTime binPath
    ph <- runBin binPath binArgs

    -- get the build/kill loop going
    buildKillLoop binPath buildCmd ph lastMod

    -- will wait for natural death or death by buildKillLoop
    waitForProcess ph
 
    -- do it all over again
    spawnLoop buildCmd binPath binArgs

-- monitor file modification time, keep trying to build and kill the ph once there is a new version
buildKillLoop :: String -> String -> ProcessHandle -> ClockTime -> IO ()
buildKillLoop binPath buildCmd ph lastMod = do
    isActive <- fmap isNothing (getProcessExitCode ph)
    -- is the process still active?
    if isActive
        then do
            buildSuccess <- buildBin buildCmd
            newMod <- getModificationTime binPath

            -- was the build successful? is there a new binary?
            if buildSuccess && (newMod /= lastMod)
                then do
                    log' "New Build Detected . . ."
                    killBin ph
                else do
                    -- wait 5 seconds before repeating
                    addTimeout 5 $ buildKillLoop binPath buildCmd ph newMod
                    return ()
        else return ()

{- meat and potatoes -}

-- does not block, returns ph
runBin :: String -> [String] -> IO ProcessHandle
runBin binPath binArgs = do
    log' "Running . . ."
    log' (showCmd binPath binArgs)
    runProcess binPath binArgs Nothing Nothing (Just stdin) (Just stdout) (Just stderr)
    where showCmd bp [] = bp
          showCmd bp ba = bp ++ " " ++ unwords ba

-- blocks until built, returns True if build was a success
buildBin :: String -> IO Bool
buildBin buildCmd = do
    (_inp,_out,err,ph) <- runInteractiveCommand buildCmd
    log' =<< hGetContents err
    waitForProcess ph
    exitCode <- getProcessExitCode ph
    return (exitCode == Just ExitSuccess)

-- does not block, kills process
killBin :: ProcessHandle -> IO ()
killBin ph = do
    log' "Killing . . ."
    terminateProcess ph
    return ()

