{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, PatternSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Happstack.Server.HStringTemplate (webST) where

import Happstack.Server
  ( ToMessage(toMessage, toContentType, toResponse)
  , Response
  )
import Control.Monad.Trans (MonadIO, liftIO)
import Text.StringTemplate
  ( STGroup
  , StringTemplate
  , directoryGroupLazy
  , getStringTemplate
  , render
  , setManyAttrib
  )
import qualified Data.ByteString.Char8 as B (pack)
import qualified Data.ByteString.Lazy.Char8 as L (pack)
import System.Log.Logger (Priority(..), logM)
log' :: Priority -> String -> IO ()
log' = logM "Happstack.Server.HStringTemplate"

-- FIXME: Caveat, assumes text/html, can this be handled at the filter level?
instance ToMessage (StringTemplate String) where
    toContentType _ = B.pack "text/plain;charset=utf-8"
    toMessage = L.pack . render

-- renders a name template with attrs
webST :: (MonadIO m) => String -> [(String, String)] -> m Response
webST name attrs = do
  grp :: STGroup String <- liftIO $ directoryGroupLazy "templates"
  case getStringTemplate name grp of
    Just tmp -> do
      -- FIXME: I would rather use show, but StringTemplate String has no Show instance
      liftIO $ log' INFO ("webST executing: " ++ name)
      return . toResponse $ setManyAttrib attrs tmp
    Nothing  -> fail $ "Template does not exist: " ++ name

