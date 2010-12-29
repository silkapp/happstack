{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module App.Control (appHandler) where

import App.View
import Control.Monad(msum)
import Control.Monad.Trans(liftIO, MonadIO)
import GuestBook
import Happstack.Server
import System.Time (getClockTime)

appHandler :: ServerPartT IO Response
appHandler = 
    do decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096) -- decode the request body if present. 
       msum [ methodM GET >> seeOther "/entries" (toResponse ()) -- matches /
            , renderFromBody "GuestBook" =<< guestBookHandler
            , dir "README" getREADME                             -- StringTemplate example
            , fileServe ["index.html"] "public"                  -- static files
            ]

getREADME :: ServerPartT IO Response
getREADME = 
    methodM GET >> liftIO getClockTime >>= renderREADME
