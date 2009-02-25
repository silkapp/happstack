module AppLogger (setupLogger) where

import System.Log.Logger
  ( Priority(..)
  , setLevel
  , setHandlers
  , getLogger
  , getRootLogger
  , saveGlobalLogger
  )
import System.Log.Handler.Simple (fileHandler, streamHandler)
import System.IO (stdout)

setupLogger = do
  logFileHandler <- fileHandler ("app.log") DEBUG
  stdoutHandler <- streamHandler stdout DEBUG

  -- Log Everything to app.log
  server <- getRootLogger
  saveGlobalLogger $ setLevel DEBUG $ setHandlers [logFileHandler] server

  -- Log Happstack.Server messages of at least INFO priority to stdout
  server <- getLogger "Happstack.Server"
  saveGlobalLogger $ setLevel INFO $ setHandlers [stdoutHandler] server
  
