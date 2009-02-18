{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module AppControl (appHandler) where

import AppState
import AppView
import Control.Applicative((<$>))
import Control.Monad(msum)
import Control.Monad.Trans(liftIO, MonadIO)
import Data.ByteString.Lazy.UTF8 (toString)
import Happstack.Data (defaultValue)
import Happstack.Server
import Happstack.Server.HSP.HTML (webHSP)
import Happstack.State (update,query)
import System.Time (getClockTime)

appHandler :: ServerPartT IO Response
appHandler = msum
  [ methodM GET >> seeOther "/entries" (toResponse ()) -- matches /
  , dir "entries" $ msum[postEntry, getEntries]        -- RESTful /entries
  , fileServe ["index.html"] "public"                  -- static files
  ]

getEntries = methodM GET >> do
  gb <- query ReadGuestBook
  webHSP $ pageFromBody "Happstack Guestbook Example" gb


postEntry = methodM POST >> do -- only accept a post method
  Just entry <- getData -- get the data
  now <- liftIO getClockTime
  update $ AddGuestBookEntry entry{date=now}
  seeOther "/entries" (toResponse ())

-- this tells happstack how to turn post data into a datatype using 'withData'
instance FromData GuestBookEntry where
  fromData = do
    author  <- toString <$> lookBS "author"
    message <- toString <$> lookBS "message"
    return $ GuestBookEntry (if (null author) then "Anonymous" else author) message defaultValue

