{-# LANGUAGE TypeSynonymInstances #-}
module Happstack.Server.ToMessage where

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.UTF8       as LU (toString, fromString)
import Happstack.Server.Monads
import Network.Wai
import Network.Wai.Enumerator (fromLBS)

class ToMessage a where
    toContentType :: a -> B.ByteString
    toContentType _ = B.pack "text/plain"
    toMessage :: a -> Either FilePath Enumerator -- C.ByteString
    toMessage = error "Happstack.Server.SimpleHTTP.ToMessage.toMessage: Not defined"
    toResponse:: a -> Response
    toResponse val =
        let bs = toMessage val
            res = Response { status           = Status200 
                           , responseHeaders = [(ContentType, toContentType val)]
                           , responseBody    = bs
                           }
        in res
{-            
        in setHeaderBS (B.pack "Content-Type") (toContentType val)
           res
-}


instance ToMessage String where
  toContentType _ = B.pack "text/plain; charset=UTF-8"
  toMessage = Right . fromLBS . LU.fromString  
  
instance ToMessage Response where
  toResponse = id
