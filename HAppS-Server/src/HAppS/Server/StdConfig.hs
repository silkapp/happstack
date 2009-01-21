{-# LANGUAGE ScopedTypeVariables #-}
module HAppS.Server.StdConfig where

import Control.Monad.Trans
import HAppS.Server.SimpleHTTP
import HAppS.Util.Daemonize
import HAppS.Server.HTTP.FileServe
import HAppS.State
--import HAppS.State.EventTH

{-
data StdConfig a = StdConfig {http::[ServerPart IO]
                             ,everySecond::IO ()
                             ,proxy :: Proxy a}

serverConfig :: StdConfig NoState
serverConfig = StdConfig {http=[]
                         ,everySecond = return ()
                         ,proxy = Proxy}

{-
class IsState st where
    getInterface :: st -> [Handler st]
-}

--data AsState a = AsState a
withState :: a -> StdConfig n -> StdConfig a
withState st s  = s{proxy = Proxy}


    

start :: forall a. (SystemState a) => StdConfig a -> IO ()
start conf = daemonize binarylocation $ 
    stdMain $ simpleHTTP (errWrap:http conf) :*: cron 1 (everySecond conf)
       :*: (End :: StdPart a)

--main = daemonize binarylocation -- restart if binary changes
--       main'

{--
these locations are used so errorwrapper can serve out compiler
error messages when you modify the code rather than forcing you
to go to the shell and do it yourself. 
--}
-}

binarylocation = "haskell/Main"
loglocation = "public/log"


errWrap :: MonadIO m => ServerPartT m Response
errWrap =  errorwrapper binarylocation loglocation
--stateFuns -- main actually has state so you can just import them
