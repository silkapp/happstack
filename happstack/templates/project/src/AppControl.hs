{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module AppControl (appHandler) where
import Control.Applicative((<$>))
import Control.Monad(msum)
import Control.Monad.Trans(liftIO, MonadIO)
import Data.ByteString.Lazy.UTF8 (toString)
import Happstack.Data (defaultValue)
import Happstack.Server
import Happstack.Server.HSP.HTML (webHSP)
import Happstack.State (update,query)
import AppState
import HSP
import System.Locale (defaultTimeLocale)
import System.Time (formatCalendarTime, getClockTime, toUTCTime)

-- for some reason I don't understand, appHandler
-- is forced to be a concrete instance.  It really wants
-- to be a ServerPartT IO Response.  Any attempt to generalize
-- it further will fail
appHandler :: [ ServerPartT IO Response ]
appHandler =
  [ dir "entries" $ msum [postEntry, getEntries]
  , dir "public"  $ fileServe ["index.html"] "public"
  , seeOther "/entries" (toResponse ())
  ]

getEntries = methodM GET >> webHSP renderGuestBook
--
-- only accept a post method for adding a new guestbook entry
postEntry = withData $ \e ->
  do
     methodM POST
     now <- liftIO $ getClockTime
     update (AddGuestBookEntry (e { date = now }))
     seeOther "/" (toResponse ())
  

-- this tells happstack how to turn post data into a datatype using 'withData'
instance FromData GuestBookEntry where
  fromData = do
    author  <- toString <$> lookBS "author"
    message <- toString <$> lookBS "message"
    return $ GuestBookEntry (if (null author) then "Anonymous" else author) message defaultValue

-- rendering details for guestbook page
renderGuestBook :: HSP XML
renderGuestBook = do
  gb <- liftIO $ query ReadGuestBook
  pageFromBody "guestbook" gb

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
        where
          p str = <p><% str %></p>
          dateStr ct = formatCalendarTime defaultTimeLocale "%a, %B %d, %Y at %H:%M:%S (UTC)" (toUTCTime ct)

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
        
pageFromBody :: (EmbedAsChild (HSPT' IO) xml) => String -> xml -> HSP XML
pageFromBody title body =
    withMetaData html4Strict $
    <html>
     <head>
      <title><% title %></title>
      <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
      <link rel="stylesheet" type="text/css" href="/public/theme/style.css" media="screen" />
     </head>
     <body>
      <div id="header">
       <div class="grunge"></div>
       <div class="peel"></div>
       <div class="topnavi">
        <ul>
         <li class="current_page_item"><a href="/" title="Guestbook">Guestbook</a></li>
        </ul>
       </div>
      </div>

      <div class="side1">
       <div class="sbar_section">
        <h2>Links</h2>
         <ul>
      	  <li class="cat-item cat-item-1"><a href="http://happstack.com/" title="happstack" accesskey="H"><span class="accesskey">H</span>appstack</a></li>
      	  <li class="cat-item cat-item-2"><a href="http://happstack.com/tutorials.html" title="happstack" accesskey="T"><span class="accesskey">T</span>utorials</a></li>
         </ul>
       </div>
      </div>

      <div class="wrap">
       <div class="innercont_main">

        <div class="post">
         <div class="posttop">
          <div class="date">14<div>Feb</div></div>
          <h1 class="posttitle">Happstack Guestbook</h1>
          <div class="storycontent">
           <p>
              Hey congrats! You're using
              <a href="http://happstack.com">Happstack</a> 0.1.9.
              This is a guestbook example which you can freely change to your
              whims and fancies.
            </p>
           <p>Leave a message for the next visitor here...</p>
           <form action="/entries" method="post" enctype="multipart/form-data:charset=UTF-8" accept-charset="UTF-8">
            <p><label for="author">A<span class="accesskey">u</span>thor</label><br /><input type="text" name="author" id="author" tabindex="1" accesskey="U" /></p>
            <p><label for="message"><span class="accesskey">M</span>essage</label><br /><textarea cols="80" rows="10" name="message" id="message" tabindex="2" accesskey="M"></textarea></p>
            <p><input type="submit" tabindex="3" accesskey="L" value="Leave GuestBook Entry" /></p>
           </form>
          </div>
         </div>
        </div>

        <% body %>
       </div>
      </div>
           
      <div class="footer">
        <div class="finalfooter">Theme : <a href="http://www.dezinerfolio.com/2007/10/10/just-another-wodpress-theme" title="sIMPRESS v2 theme">sIMPRESS v2</a> by <a href="http://dezinerfolio.com" title="Dezinerfolio">Dezinerfolio</a></div>
      </div>

     </body>
    </html>
