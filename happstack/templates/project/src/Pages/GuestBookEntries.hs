{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Pages.GuestBookEntries 
     ( guestBookEntriesPage 
     ) where

import HSP
import Happstack.Server  (Method(GET), Response, ServerPart, ServerPartT, methodM)
import Happstack.State   (query)
import Pages.AppTemplate (appTemplate)
import Pages.Common      (dateStr)
import State.GuestBook   (ReadGuestBook(..))
import Types.GuestBook   (GuestBook(..), GuestBookEntry(..))

guestBookEntriesPage :: ServerPart Response
guestBookEntriesPage =
    do methodM GET
       gb  <- query ReadGuestBook
       appTemplate "Guestbook Entries" () (guestBookXML gb)

guestBookXML :: GuestBook -> XMLGenT (ServerPartT IO) XML
guestBookXML (GuestBook entries) =
    <div>
     <h2 id="comments" class="h2comment">Words of Wisdom</h2>
     <div class="clear" />
     <ul class="commentlist">
      <% mapM guestBookEntryXML $ zip entries (cycle [False,True]) %>
     </ul>
    </div>

guestBookEntryXML :: (GuestBookEntry, Bool) -> XMLGenT (ServerPartT IO) XML
guestBookEntryXML ((GuestBookEntry author message date email), alt) =
           <li class=(if alt then "alt" else "")>
            <strong><% author ++ (displayemail email)  %></strong> said:<br /><br />
            <% map p (lines message) %>
            <br />
            <small class="commentmetadata"><% dateStr date %></small> 
           </li>
        where p str = <p><% str %></p>
              displayemail "" = ""
              displayemail x = "<" ++ x ++ ">"


