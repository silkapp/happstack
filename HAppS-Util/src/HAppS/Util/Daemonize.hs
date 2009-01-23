module HAppS.Util.Daemonize where 

import System.Directory
import System.Environment
import System.Exit
import System.Time
import Control.Concurrent
import Control.Exception
import Control.Monad.Error
import HAppS.Crypto.SHA1
import HAppS.Util.Common


{--
  1. don't start the app if already running. the app is already running if something
  has written to the daemon file recently
  
  2. kill the app if the binary has changed since the app started
--}

-- Will placing the lock-file in the current directory work if we run the application from cron?
daemonize binarylocation main = 
    do
    startTime <- getClockTime
    tid1 <- exitIfAlreadyRunning startTime
    mId <- myThreadId
    tid2 <- appCheck binarylocation startTime mId
    main `finally` (mapM killThread [tid1,tid2])
    where 
    seconds n = noTimeDiff { tdSec = n }
    exitIfAlreadyRunning startTime = 
        do
        uniqueId <- getDaemonizedId
        let name = ".haskell_daemon." ++ uniqueId
        fe <- doesFileExist name
        when fe $ 
             do 
             daemonTime <- getModificationTime name         
             when (diffClockTimes startTime daemonTime < seconds 2) $
                  exitWith ExitSuccess  >> return ()
        periodic (repeat 1) $ writeFile name "daemon" 

    appCheck bl startTime mId = periodic (repeat 1) $ 
        do 
        fe <- doesFileExist bl
        if not fe then return () else do
        appModTime <- getModificationTime bl
        if startTime < appModTime then 
             throwTo mId $ 
#ifndef EXTENSIBLE_EXCEPTIONS
                            ExitException 
#endif
                              ExitSuccess -- throws to the main thread
--           raiseSignal softwareTermination -- can we eliminate posix dependency?
           else do
        return ()

{--
          appCheck Nothing   _     = return ()
          appCheck (Just bl) start = do at <- try $ getModificationTime bl
                                        case at of
                                          Right appModTime | start < appModTime
                                              -> raiseSignal softwareTermination
                                          _   -> return ()
--}

--daemonize = daemonize' Nothing
getDaemonizedId
    = do prog <- getProgName
         args <- getArgs
         return (sha1 (prog ++ unwords args))

