-- | support for using HSP+Happstack for rendering HTML 
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans -F -pgmFtrhsx #-}
module Happstack.Server.HSP.HTML
  ( defaultTemplate
  , webHSP
  , webHSP'
  , module HSP
  ) where

import Control.Monad.Trans (MonadIO(), liftIO)
import qualified Data.ByteString.Char8 as P
import qualified Data.ByteString.Lazy.UTF8 as L
import Control.Monad (liftM)
import Happstack.Server
  ( ToMessage(toMessage, toContentType, toResponse)
  , Response
  )
import HSP
import HSP.ServerPartT ()
import qualified HSX.XMLGenerator as HSX

instance ToMessage XML where
    toContentType _ = P.pack "text/html;charset=utf-8"
    toMessage xml   = toMessage (html4Strict, xml)

instance ToMessage (Maybe XMLMetaData, XML) where
    toContentType (Just md,_) = P.pack (contentType md)
    toContentType _ = P.pack "text/html;charset=utf-8"
    toMessage (Just (XMLMetaData (showDt, dt) _ pr), xml) = 
        L.fromString ((if showDt then (dt ++) else id) (pr xml))
    toMessage (Nothing, xml) =
        L.fromString (renderAsHTML xml)

-- | A generic webpage template
defaultTemplate :: (XMLGenerator m, EmbedAsChild m headers, EmbedAsChild m body) => 
                   String   -- ^ text to use in \<title\> tag
                -> headers  -- ^ extra headers to insert in \<head\> tag. Use @()@ if none.
                -> body     -- ^ content to put between the \<body\> tags.
                -> m (HSX.XML m)
defaultTemplate title headers body =
    unXMLGenT $
    <html>
     <head>
      <title><% title %> </title>
      <% headers %>
     </head>
     <body>
      <% body %>
     </body>
    </html>

-- | Converts a @HSP XML@ to a Happstack 'Response'.
-- Since @HSP XML@ is the type returned by using literal HTML syntax
-- with HSP, you can wrap up your HTML as webHSP $ \<html\>...\</html\>
-- to use it with Happstack.
webHSP :: (MonadIO m) => HSP XML -> m Response
webHSP = webHSP' Nothing

-- | webHSP with 'XMLMetaData'
webHSP' :: (MonadIO m) => Maybe XMLMetaData -> HSP XML -> m Response
webHSP' metadata hsp = toResponse `liftM` liftIO (evalHSP metadata hsp)
