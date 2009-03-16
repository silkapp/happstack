{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module GuestBook.Control where

import Control.Applicative((<$>))
import Control.Monad(msum)
import Control.Monad.Trans(liftIO)
import Data.ByteString.Lazy.UTF8 (toString)
import GuestBook.State (GuestBookEntry(..),AddGuestBookEntry(..),ReadGuestBook(..))
import GuestBook.View
import Happstack.Server
import Happstack.Data(defaultValue)
import Happstack.State(query,update)
import HSP
import System.Time(getClockTime)

guestBookHandler :: ServerPartT IO (HSP XML)
guestBookHandler =
  dir "entries" $ msum [postEntry, getEntries]        -- RESTful /entries

postEntry :: ServerPartT IO (HSP XML)
postEntry = methodM POST >> do -- only accept a post method
  Just entry <- getData -- get the data
  now <- liftIO getClockTime
  update $ AddGuestBookEntry entry{date=now}
  seeOther "/entries" (seeOtherXML "/entries")

-- |show all the entries in the guestbook
-- argument is a callback function 
getEntries :: ServerPartT IO (HSP XML)
getEntries = 
    methodM GET >> 
                do gb  <- query ReadGuestBook
                   ok $ <div><% gb %></div> -- FIXME: remove <div />

-- this tells happstack how to turn post data into a datatype using 'withData'
instance FromData GuestBookEntry where
  fromData = do
    author  <- look "author"
    message <- look "message"
    return $ GuestBookEntry (if (null author) then "Anonymous" else author) message defaultValue
