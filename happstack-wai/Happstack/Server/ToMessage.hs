{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Happstack.Server.ToMessage where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.UTF8       as LU (fromString)
import Network.Wai
import Network.Wai.Enumerator (fromLBS)

class ToMessage a where
    toContentType :: a -> B.ByteString
    toContentType _ = B.pack "text/plain"
    toMessage :: a -> ResponseBody
    toMessage = error "Happstack.Server.SimpleHTTP.ToMessage.toMessage: Not defined"
    toResponse:: a -> Response
    toResponse val =
        let bs = toMessage val
            res = Response { status           = status200
                           , responseHeaders = [("Content-Type", toContentType val)]
                           , responseBody    = bs
                           }
        in res
{-            
        in setHeaderBS (B.pack "Content-Type") (toContentType val)
           res
-}


instance ToMessage String where
  toContentType _ = B.pack "text/plain; charset=UTF-8"
  toMessage = ResponseLBS . LU.fromString  
  
instance ToMessage Response where
  toResponse = id
