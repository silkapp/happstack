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
import Text.Hamlet.Monad (hamletToText, liftHamlet)
import Happstack.Server


-- |turn a 'Hamlet' template into a Happstack 'Response'
hamletToResponse :: (Monad m) => 
                    (url -> String) -> -- ^ function to 'url' values in the template into their 'String' representation
                    Hamlet url m () -> -- ^ a 'Hamlet' template
                    m Response
hamletToResponse showFn hamlet = 
    do msg <- hamletToText showFn hamlet
       return $ toResponse_ (B.pack "text/html; charset=UTF-8") (T.encodeUtf8 msg)

-- available as toResponseBS in happstack-server >= 0.5
toResponse_ :: B.ByteString -> L.ByteString -> Response
toResponse_ contentType message =
    let res = Response 200 M.empty nullRsFlags message Nothing
    in setHeaderBS (B.pack "Content-Type") contentType res

mapHamlet :: forall a b url m. (Monad m) => (m a -> m b) -> Hamlet url m a -> Hamlet url m b
mapHamlet f m = 
    Hamlet $ \showFn seed iteratee -> f' (runHamlet m showFn seed iteratee)
        where
          f' :: m (Either seed (a, seed)) -> m (Either seed (b, seed))
          f' m =
              do e <- m
                 case e of
                   (Left s) -> return (Left s)
                   (Right (a, seed)) -> 
                       do b <- f (return a)
                          return (Right (b, seed))

instance (ServerMonad m) => ServerMonad (Hamlet url m) where
    askRq   = liftHamlet askRq
    localRq f = mapHamlet (localRq f)

instance (FilterMonad r m) => FilterMonad r (Hamlet url m) where
    setFilter     = liftHamlet . setFilter
    composeFilter = liftHamlet . composeFilter
    getFilter     = mapHamlet getFilter

instance (WebMonad a m) => WebMonad a (Hamlet url m) where
    finishWith = liftHamlet . finishWith


