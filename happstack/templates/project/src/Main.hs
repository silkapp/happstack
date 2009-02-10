module Main where
import Control.Concurrent
  (forkIO
  ,killThread)
import Happstack.Server
  (simpleHTTP
  ,nullConf
  ,waitForTermination)
import Happstack.State
  (Proxy(..)
   ,startSystemState
   ,shutdownSystem
   ,createCheckpoint)
import AppLogger
  (setupLogger)
import AppState
  (AppState(..))
import AppControl
  (appHandler)

stateProxy :: Proxy AppState
stateProxy = Proxy

main = do
  setupLogger
  
  -- start the state system
  control <- startSystemState stateProxy
  createCheckpoint control
  
  -- start the http server
  http_tid <- forkIO $ simpleHTTP nullConf appHandler
  
  -- wait for termination signal
  waitForTermination
  
  -- cleanup
  killThread http_tid
  shutdownSystem control 

