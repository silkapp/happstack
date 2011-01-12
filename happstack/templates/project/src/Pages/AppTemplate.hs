{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Pages.AppTemplate where

import Control.Applicative ((<$>))
import HSP
import Happstack.Server    (ServerPart, ServerPartT, Response, toResponse)
import HSP.ServerPartT     () -- instance XMLGenerator (ServerPartT m)
import Happstack.Server.HSP.HTML ()

appTemplate ::
    ( EmbedAsChild (ServerPartT IO) headers
    , EmbedAsChild (ServerPartT IO) body
    ) =>
    String -> headers -> body -> ServerPart Response
appTemplate title headers body = toResponse <$> (unXMLGenT (appTemplate' title headers body))

appTemplate' :: 
    ( EmbedAsChild (ServerPartT IO) headers
    , EmbedAsChild (ServerPartT IO) body
    ) =>
    String -> headers -> body -> XMLGenT (ServerPartT IO) XML
appTemplate' title headers body =
    <html>
     <head>
      <title><% title %></title>
      <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
      <link rel="stylesheet" type="text/css" href="/theme/style.css" media="screen" />
      <% headers %>
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
      	  <li class="cat-item cat-item-2"><a href="http://happstack.com/docs" title="happstack" accesskey="T"><span class="accesskey">T</span>utorials</a></li>
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
              <a href="http://happstack.com">Happstack</a>.
              This is a guestbook example which you can freely change to your
              whims and fancies.
            </p>
            <p>
              This page is written using Haskell Server Pages (HSP). For an example
              of a page using HStringTemplate, look at the
              <a href="/README">dynamic README</a>.
            </p>
           <p>Leave a message for the next visitor here...</p>
           <form action="/guestbook" method="post" enctype="multipart/form-data;charset=UTF-8" accept-charset="UTF-8">
            <p><label for="author">A<span class="accesskey">u</span>thor</label><br /><input type="text" name="author" id="author" tabindex="1" accesskey="U" />
            </p>
            <p><label for="message"><span class="accesskey">M</span>essage</label><br /><textarea cols="80" rows="10" name="message" id="message" tabindex="2" accesskey="M"></textarea></p>
            <p><label for="email"><span class="accesskey">E</span>mail</label><br /><input type="text" name="email" id="email" tabindex="3" accesskey="E" />
            </p>
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


