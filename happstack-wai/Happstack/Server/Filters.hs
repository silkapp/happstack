{-# LANGUAGE FlexibleContexts #-}
module Happstack.Server.Filters where

import Network.Wai hiding (Request)
import Happstack.Server.Monads
import Data.Monoid (mempty)

-- | sets the return code in your response
setResponseCode :: FilterMonad Response m => Int -> m ()
setResponseCode = setResponseStatus . toStatus

setResponseStatus :: FilterMonad Response m => Status -> m ()
setResponseStatus stat
    = composeFilter $ \r -> r{status = stat}

toStatus :: Int -> Status
toStatus 200 = Status200
toStatus 301 = Status301
toStatus 302 = Status302
toStatus 303 = Status303
toStatus 400 = Status400
toStatus 401 = Status401
toStatus 403 = Status403
toStatus 404 = Status404
toStatus 405 = Status405
toStatus 500 = Status500
toStatus n   = Status n mempty

-- | same as setResponseCode status >> return val
resp :: (FilterMonad Response m) => Int -> b -> m b
resp code val = setResponseCode code >> return val

-- | Respond with @200 OK@.
ok :: (FilterMonad Response m) => a -> m a
ok = resp 200

-- | Respond with @500 Interal Server Error@
internalServerError :: (FilterMonad Response m) => a -> m a
internalServerError = resp 500

-- | Responds with @502 Bad Gateway@
badGateway :: (FilterMonad Response m) => a -> m a
badGateway = resp 502

-- | Respond with @400 Bad Request@.
badRequest :: (FilterMonad Response m) => a -> m a
badRequest = resp 400

-- | Respond with @401 Unauthorized@.
unauthorized :: (FilterMonad Response m) => a -> m a
unauthorized = resp 401

-- | Respond with @403 Forbidden@.
forbidden :: (FilterMonad Response m) => a -> m a
forbidden = resp 403

-- | Respond with @404 Not Found@.
notFound :: (FilterMonad Response m) => a -> m a
notFound = resp 404
