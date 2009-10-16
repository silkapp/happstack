{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Happstack.Server.HSX where

import Happstack.Server.SimpleHTTP (ServerMonad(..), FilterMonad(..), WebMonad(..))
import HSX.XMLGenerator (XMLGenT(..))

instance (ServerMonad m) => ServerMonad (XMLGenT m) where
    askRq = XMLGenT askRq
    localRq f (XMLGenT m) = XMLGenT (localRq f m)

instance (FilterMonad a m) => FilterMonad a (XMLGenT m) where
    setFilter = XMLGenT . setFilter
    composeFilter f = XMLGenT (composeFilter f)
    getFilter (XMLGenT m) = XMLGenT (getFilter m)

instance (WebMonad a m) => WebMonad a (XMLGenT m) where
    finishWith r = XMLGenT $ finishWith r
