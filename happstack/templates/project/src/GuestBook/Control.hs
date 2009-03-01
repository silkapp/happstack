{-# LANGUAGE FlexibleContexts #-}
module GuestBook.Control where

import Control.Applicative((<$>))
import Control.Monad(msum)
import Control.Monad.Trans(liftIO)
import Data.ByteString.Lazy.UTF8 (toString)
import GuestBook.State (GuestBook(..),GuestBookEntry(..),AddGuestBookEntry(..),ReadGuestBook(..))
import Happstack.Server
import Happstack.Data(defaultValue)
import Happstack.State(query,update)
import HSP
import System.Time(getClockTime)

guestBookHandler :: (String -> GuestBook -> ServerPartT IO Response) -> ServerPartT IO Response
guestBookHandler renderFromBody =
  dir "entries" $ msum [postEntry, getEntries renderFromBody]        -- RESTful /entries

postEntry :: ServerPartT IO Response
postEntry = methodM POST >> do -- only accept a post method
  Just entry <- getData -- get the data
  now <- liftIO getClockTime
  update $ AddGuestBookEntry entry{date=now}
  seeOther "/entries" (toResponse ())

-- |show all the entries in the guestbook
-- argument is a callback function 
getEntries :: (String -> GuestBook -> ServerPartT IO Response) -> ServerPartT IO Response
getEntries renderFromBody = 
    methodM GET >> 
            do gb <- query ReadGuestBook
               renderFromBody "Happstack Guestbook Example" gb

{-
  gb <- 
  renderFromBody "Happstack Guestbook Example" gb
 -}

-- this tells happstack how to turn post data into a datatype using 'withData'
instance FromData GuestBookEntry where
  fromData = do
    author  <- toString <$> lookBS "author"
    message <- toString <$> lookBS "message"
    return $ GuestBookEntry (if (null author) then "Anonymous" else author) message defaultValue

