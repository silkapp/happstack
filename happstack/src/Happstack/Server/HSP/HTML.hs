{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Happstack.Server.HSP.HTML
  ( webHSP
  ) where

import Control.Monad.Trans (MonadIO(), liftIO)
import qualified Data.ByteString.Char8 as P
import qualified Data.ByteString.Lazy.UTF8 as L
import Happstack.Server
  ( ToMessage(toMessage, toContentType, toResponse)
  , Response()
  )
import HSP
  ( HSP()
  , XML()
  , XMLMetaData(XMLMetaData, contentType)
  , evalHSP
  , html4Strict
  , renderAsHTML
  )

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

-- | Converts a @HSP XML@ to a Happstack Response.
-- Since @HSP XML@ is the type returned by using literal HTML syntax
-- with HSP, you can wrap up your HTML as webHSP $ <html>...</html>
-- to use it with Happstack.
webHSP :: (MonadIO m) => HSP XML -> m Response
webHSP hsp = return . toResponse =<< liftIO (evalHSP Nothing hsp)

