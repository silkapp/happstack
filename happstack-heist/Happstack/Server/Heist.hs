{-# LANGUAGE FlexibleInstances #-}
-- | functions for using Heist with Happstack
module Happstack.Server.Heist 
    ( templateReloader
    , templateServe
    , render
    ) where

import Control.Monad                           (MonadPlus(mzero), msum)
import Control.Monad.Trans                     (MonadIO)
import           Data.ByteString.Char8         (ByteString)
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy          as L
import Happstack.Server                        (Response, ServerMonad, askRq, nullDir, rqUri, toResponseBS)
import Text.Templating.Heist                   (renderTemplate)
import Text.Templating.Heist.TemplateDirectory (TemplateDirectory, getDirectoryTS, reloadTemplateDirectory)
import qualified  Text.XML.Expat.Tree          as X

templateReloader :: (MonadIO m, MonadIO n) => 
                    TemplateDirectory m 
                 -> n Response
templateReloader td = do
    e <- reloadTemplateDirectory td
    return $ toResponseBS (B.pack "text/plain; charset=utf-8") $
        L.fromChunks [either B.pack (const $ B.pack "Templates loaded successfully.") e]

templateServe :: (ServerMonad m, MonadPlus m, MonadIO m) =>
                 TemplateDirectory m 
              -> m Response
templateServe td = 
    msum [ nullDir >> render td (B.pack "index")
         , render td . B.pack . rqUri =<< askRq
         ]

render:: (MonadPlus m, MonadIO m) => 
         TemplateDirectory m 
      -> ByteString 
      -> m Response
render td template = do
    ts    <- getDirectoryTS td
    bytes <- renderTemplate ts template
    flip (maybe mzero) bytes $ \x -> do
        return (toResponseBS (B.pack "text/html; charset=utf-8") (L.fromChunks [x]))
