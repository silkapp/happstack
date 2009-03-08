module AppLogger (setupLogger) where

import System.Log.Logger
    ( Priority(..)
    , rootLoggerName
    , setLevel
    , setHandlers
    , updateGlobalLogger
    )
import System.Log.Handler.Simple (fileHandler, streamHandler)
import System.IO (stdout)

setupLogger = do
    appLog <- fileHandler ("app.log") INFO
    accessLog <- fileHandler ("access.log") NOTICE
    stdoutLog <- streamHandler stdout NOTICE

    -- Root Log
    updateGlobalLogger
        rootLoggerName
        (setLevel DEBUG . setHandlers [appLog])

    -- Access Log
    updateGlobalLogger
        "Happstack.Server.AccessLog.Combined"
        (setLevel NOTICE . setHandlers [accessLog])

    -- Server Log
    updateGlobalLogger
        "Happstack.Server"
        (setLevel INFO . setHandlers [stdoutLog])

