{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module App.Control (appHandler) where

import App.View
import Control.Monad(msum)
import Control.Monad.Trans(liftIO, MonadIO)
import GuestBook
import Happstack.Server
import System.Time (getClockTime)

appHandler :: ServerPartT IO Response
appHandler = msum
  [ methodM GET >> seeOther "/entries" (toResponse ()) -- matches /
  , guestBookHandler renderFromBody
  , dir "README" getREADME                             -- StringTemplate example
  , fileServe ["index.html"] "public"                  -- static files
  ]

getREADME :: ServerPartT IO Response
getREADME = methodM GET >> do
  now <- liftIO getClockTime
  renderREADME now
