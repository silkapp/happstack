{-# LANGUAGE CPP, EmptyDataDecls #-}

module Happstack.State.Control
    ( startSystemState
    , startSystemStateAmazon
    , stdSaver
    , waitForTermination
    ) where

import System.Log.Logger
import qualified System.Log.Handler as SLH
import System.Log.Handler.Simple
import System.Log.Handler.Syslog

import Control.Monad
import Data.List
import Data.Char
import Data.Maybe
import System.Environment
import System.IO
import System.Exit
import System.Console.GetOpt
import Control.Monad.Trans
import Control.Concurrent

#ifdef UNIX
import System.Posix.Signals hiding (Handler)
import System.Posix.IO ( stdInput )
import System.Posix.Terminal ( queryTerminal )
#endif

import Happstack.State.Transaction
import Happstack.State.Saver
import Happstack.State.CentralLogServer (ApplicationName)
import Happstack.State.TxControl
import Happstack.State.ComponentSystem
import Happstack.Data.Proxy hiding (proxy)

-- | Starts the MACID system without multimaster support.  Uses the default behavior
-- of saving the state into the _local directory.
startSystemState :: (Methods a, Component a) =>
                    Proxy a -> IO (MVar TxControl)
startSystemState proxy
    = do _txConfig <- parseArgs
         saver <- stdSaver
         runTxSystem saver proxy

startSystemStateAmazon :: (Methods a, Component a) => ApplicationName -> Proxy a -> IO (MVar TxControl)
startSystemStateAmazon appName proxy
    = do _txConfig <- parseArgs
         runTxSystemAmazon appName proxy

-- | Returns the default Saver.  It will save the application state into
-- the _local directory.
stdSaver :: IO Saver
stdSaver = do pn <- getProgName
              return $ Queue (FileSaver ("_local/" ++pn++"_state"))

-- | Wait for a signal.
--   On unix, a signal is sigINT or sigTERM. On windows, the signal
--   is entering 'e'.
waitForTermination :: IO ()
waitForTermination
    = do
#ifdef UNIX
         istty <- queryTerminal stdInput
         mv <- newEmptyMVar
         installHandler softwareTermination (CatchOnce (putMVar mv ())) Nothing
         case istty of
           True  -> do installHandler keyboardSignal (CatchOnce (putMVar mv ())) Nothing
                       return ()
           False -> return ()
         takeMVar mv
#else
         let loop 'e' = return () 
             loop _   = getChar >>= loop
         loop 'c'
#endif

mkTxConfig :: [Flag] -> TxConfig
mkTxConfig = foldr worker nullTxConfig
    where worker (Cluster serv) c = c{txcOperationMode = ClusterMode (fromMaybe "" serv)}
          worker (ClusterPort port) c = c{txcClusterPort = port}
          worker _ c = c

data NullLogger
instance SLH.LogHandler NullLogger where
    setLevel = error "Don't do this! This logger should not be used!"
    getLevel = error "Don't do this! This logger should not be used!"
    emit = error "Don't do this! This logger should not be used!"
    close = error "This logger should not be used!"

setLoggingSettings :: [Flag] -> IO ()
setLoggingSettings flags = do updateGlobalLogger "" (setHandlers ([] :: [NullLogger]))
                              s <- streamHandler stdout DEBUG
                              updateGlobalLogger "Happstack" (setHandlers [s] . setLevel WARNING)
                              mapM_ worker flags
    where worker (LogTarget SysLog)  = do s <- openlog "Happstack" [PID] DAEMON DEBUG -- This priority seems to be ignored?
                                          updateGlobalLogger "Happstack" (setHandlers [s])
          worker (LogTarget StdOut)  = do s <- streamHandler stdout DEBUG
                                          updateGlobalLogger "Happstack" (setHandlers [s])
          worker (LogTarget (File path)) = do s <- fileHandler path DEBUG  -- This priority seems to be ignored?
                                              updateGlobalLogger "Happstack" (setHandlers [s])
          worker (LogLevel pri) = do updateGlobalLogger "Happstack" (setLevel pri)
                                     rlogger <- getLogger "Happstack"
                                     logM "" WARNING ("Set logging priority to " ++ show (getLevel rlogger))
          worker _ = return ()

-- order should not matter, though it does now.
-- we should ALSO allow multiple loggers at the same time! --log-target=stdout --log-target=syslog
options :: [OptDescr Flag]
options = [Option "" ["log-level"] (ReqArg (LogLevel . read . map toUpper) "level")
                      "Log level: DEBUG, INFO, NOTICE, WARNING, ERROR, CRITICAL, ALERT, EMERGENCY. Default: WARNING"
          ,Option "" ["log-target"] (ReqArg (LogTarget . readTarget) "target")
                      "Log target: stdout, syslog, or a FilePath such as /home/foo/bar.log . Default: stdout"
          ]

data Target = File FilePath | StdOut | SysLog deriving (Read,Show,Eq,Ord)

data Flag = LogLevel Priority | LogTarget Target
          | Cluster (Maybe String) | ClusterPort Int deriving Show

readTarget :: String -> Target
readTarget arg = case map toLower arg of
                   "stdout" -> StdOut
                   "syslog" -> SysLog
                   _        -> File arg

castOptions :: [OptDescr ()]
castOptions = flip map options $ \(Option c f desc help) -> Option c f (worker desc) help
              where worker (NoArg _) = (NoArg ())
                    worker (ReqArg _ f) = ReqArg (const ()) f
                    worker (OptArg _ f) = OptArg (const ()) f

parseArgs :: IO TxConfig
parseArgs = do
    args <- liftIO getArgs
    pn   <- liftIO getProgName
    let err n ls = -- XXX these next lines should be written to stderr!
                   do putStrLn ("Syntax error in command line - "++n)
                      putStrLn $ unlines $ map ("    "++) ls
                      putStrLn ("Usage "++usageInfo pn castOptions)
                      exitWith (ExitFailure 1)
    case getOpt' Permute options args of
      (flags,_fs,_args',[]) -> do setLoggingSettings flags
                                  -- FIXME: replace system args with fs++args'
                                  return (mkTxConfig flags)
      (_,_,_,es)          -> err "errors" es

