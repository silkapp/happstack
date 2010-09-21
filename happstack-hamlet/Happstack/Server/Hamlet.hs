{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Happstack.Server.Hamlet 
    ( hamletToResponse 
    )
    where

import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map                   as M
import qualified Data.Text.Lazy.Encoding    as T    

import Text.Hamlet
import Happstack.Server


-- |turn a 'Hamlet' template into a Happstack 'Response'
hamletToResponse :: (url -> [(String, String)] -> String) -> -- ^ function to 'url' values in the template into their 'String' representation
                    Hamlet url -> -- ^ a 'Hamlet' template
                    Response
hamletToResponse showFn hamlet = 
    let msg = renderHamlet showFn hamlet
     in toResponse_ (B.pack "text/html; charset=UTF-8") msg

-- available as toResponseBS in happstack-server >= 0.5
toResponse_ :: B.ByteString -> L.ByteString -> Response
toResponse_ contentType message =
    let res = Response 200 M.empty nullRsFlags message Nothing
    in setHeaderBS (B.pack "Content-Type") contentType res
