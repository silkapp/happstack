{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module GuestBook.View where

import GuestBook.State (GuestBook(..),GuestBookEntry(..))
import HSP
import qualified HSX.XMLGenerator as HSX (XML)
import System.Locale (defaultTimeLocale)
import System.Time(ClockTime(..), formatCalendarTime, toUTCTime)

-- * Convenience Functions

dateStr :: ClockTime -> String
dateStr ct =
  formatCalendarTime
    defaultTimeLocale
    "%a, %B %d, %Y at %H:%M:%S (UTC)"
    (toUTCTime ct)

-- * Main Implementation

instance (XMLGenerator m) => (EmbedAsChild m (GuestBookEntry, Bool)) where
    asChild ((GuestBookEntry author message date), alt) =
        <%
           <li class=(if alt then "alt" else "")>
            <strong><% author %></strong> said:<br /><br />
            <% map p (lines message) %>
            <br />
            <small class="commentmetadata"><% dateStr date %></small> 
           </li>
         %>
        where p str = <p><% str %></p>

instance (XMLGenerator m) => (EmbedAsChild m GuestBook) where
    asChild (GuestBook entries) = 
        <% 
         <div>
          <h2 id="comments" class="h2comment">Words of Wisdom</h2>
          <div class="clear" />
          <ul class="commentlist">
           <% zip entries (cycle [False,True]) %>
          </ul>
         </div>
        %>

seeOtherXML :: (XMLGenerator m) => String -> XMLGenT m (HSX.XML m)
seeOtherXML url = <a href=url alt="303 see other"><% url %></a>