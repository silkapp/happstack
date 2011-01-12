module Pages.PostEntry (postEntryPage) where

import Control.Monad.Trans (liftIO)
import Happstack.Server    (Method(POST), ServerPart, Response, look, methodM, seeOther, toResponse)
import Happstack.State     (update)
import State.GuestBook     (AddGuestBookEntry(..))
import System.Time         (getClockTime)
import Types.GuestBook     (GuestBookEntry(..))

postEntryPage :: ServerPart Response
postEntryPage =
    do methodM POST -- only accept a post method
       author  <- look "author"  -- `mplus` (error "GuestBookEntry, need author")
       message <- look "message" --  `mplus` (error "GuesBookEntry, need message")
       email   <- look "email"   -- `mplus` (error "GuestBookEntry: need email")
       now <- liftIO getClockTime
       let entry = GuestBookEntry { author  = (if null author then "Anonymous" else author) 
                                  , message = message 
                                  , date    = now 
                                  , email   = email
                                  }
       update $ AddGuestBookEntry entry
       seeOther "/guestbook" (toResponse "/guestbook")
