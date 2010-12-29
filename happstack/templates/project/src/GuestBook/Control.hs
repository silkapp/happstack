{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module GuestBook.Control where

import Control.Applicative((<$>))
import Control.Monad(msum)
import Control.Monad.Trans(liftIO)
import Data.ByteString.Lazy.UTF8 (toString)
import GuestBook.State2 (GuestBookEntry(..),AddGuestBookEntry(..),ReadGuestBook(..))
import GuestBook.View
import Happstack.Server
import Happstack.Data(defaultValue)
import Happstack.State(query,update)
import HSP
import System.Time(getClockTime)
import Control.Monad

guestBookHandler :: ServerPartT IO (HSP XML)
guestBookHandler =
  dir "entries" $ msum [postEntry, getEntries]        -- RESTful /entries

postEntry :: ServerPartT IO (HSP XML)
postEntry =  do 
  methodM POST -- only accept a post method
  mbEntry <- getData 
  case mbEntry of 
    (Left e) -> badRequest $ <p><% e %></p>
    (Right entry) -> do
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
    author  <- look "author" `mplus` (error "GuestBookEntry, need author")
    message <- look "message" `mplus` (error "GuesBookEntry, need message")
    email   <- look "email" `mplus` (error "GuestBookEntry: need email")
    return $ GuestBookEntry (if null author then "Anonymous" else author) message defaultValue email
