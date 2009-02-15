module AppLogger (setupLogger) where
import System.Log.Logger
  (Priority(..)
  ,updateGlobalLogger
  ,setLevel
  ,setHandlers)
import System.Log.Handler.Simple
  (fileHandler)

setupLogger progName = do
  h <- logHandler progName
  updateGlobalLogger "Happstack.Server" (setLevel DEBUG . setHandlers [h])
  updateGlobalLogger "Happstack.State" (setLevel DEBUG . setHandlers [h])
  
logHandler progName = fileHandler (progName ++ ".log") DEBUG

