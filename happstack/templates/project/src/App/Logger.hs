module App.Logger (LoggerHandle, setupLogger, teardownLogger) where

import System.Log.Logger
    ( Priority(..)
    , rootLoggerName
    , setLevel
    , setHandlers
    , updateGlobalLogger
    )
import System.Log.Handler (close)
import System.Log.Handler.Simple (GenericHandler, fileHandler, streamHandler)
import System.IO (stdout, Handle)

-- | Opaque type covering all information needed to teardown the logger.
data LoggerHandle = LoggerHandle { 
    rootLogHandler   :: GenericHandler Handle
  , accessLogHandler :: GenericHandler Handle
  , serverLogHandler :: GenericHandler Handle
  }

setupLogger = do
    appLog <- fileHandler "app.log" INFO
    accessLog <- fileHandler "access.log" INFO
    stdoutLog <- streamHandler stdout NOTICE

    -- Root Log
    updateGlobalLogger
        rootLoggerName
        (setLevel DEBUG . setHandlers [appLog])

    -- Access Log
    updateGlobalLogger
        "Happstack.Server.AccessLog.Combined"
        (setLevel INFO . setHandlers [accessLog])

    -- Server Log
    updateGlobalLogger
        "Happstack.Server"
        (setLevel NOTICE . setHandlers [stdoutLog])
    -- return opaque AppLogger handle
    return $ LoggerHandle appLog accessLog stdoutLog

-- | Tear down the application logger; i.e. close all associated log handlers.
teardownLogger handle = do
  close $ serverLogHandler handle
  close $ accessLogHandler handle
  close $ rootLogHandler   handle

  
  
  
