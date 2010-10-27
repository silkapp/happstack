{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}
-- | Functions for creating, adding, and expiring cookies. To lookup cookie values see "Happstack.Server.RqData".
module Happstack.Server.Cookie
    ( Cookie(..)
    , CookieLife(..)
    , mkCookie
    , addCookie
    , addCookies
    , expireCookie
    )
    where

import Control.Monad.Trans              (MonadIO(..))
import Data.Data                        (Data)
import Data.Typeable                    (Typeable)
import Data.Time.Clock                  (UTCTime)
import Happstack.Server.Monads          (FilterMonad, composeFilter)
import Happstack.Server.Internal.Cookie (Cookie(..), CookieLife(..), calcLife, mkCookie, mkCookieHeader)
import Happstack.Server.Types           (Response, addHeader)
import Happstack.Util.Common            (Seconds)

-- | Add the cookie with a timeout to the response.
addCookie :: (MonadIO m, FilterMonad Response m) => CookieLife -> Cookie -> m ()
addCookie life cookie =
    do l <- liftIO $ calcLife life
       (addHeaderM "Set-Cookie") $ mkCookieHeader l cookie
    where
      addHeaderM a v = composeFilter $ \res-> addHeader a v res

-- | Add the list of cookie timeout pairs to the response.
addCookies :: (MonadIO m, FilterMonad Response m) => [(CookieLife, Cookie)] -> m ()
addCookies = mapM_ (uncurry addCookie)

-- | Expire the cookie immediately and set the cookie value to @\"\"@
expireCookie :: (MonadIO m, FilterMonad Response m) => String -> m () 
expireCookie cookieName = addCookie Expired (mkCookie cookieName "")
