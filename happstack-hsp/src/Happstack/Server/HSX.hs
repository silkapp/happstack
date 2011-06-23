-- | This module contains orphan 'XMLGenT' instances for 'ServerMonad', 'FilterMonad', 'WebMonad', 'HasRqData', and 'Happstack'. It does not export any functions.
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Happstack.Server.HSX where

import Control.Applicative         (Alternative(..))
import Control.Monad               (MonadPlus(..))
import Control.Monad.Trans         (MonadIO(..))
import Happstack.Server.SimpleHTTP (ServerMonad(..), FilterMonad(..), WebMonad(..), HasRqData(..), Happstack(..), Response)
import HSX.XMLGenerator            (XMLGenT(..))

instance (ServerMonad m) => ServerMonad (XMLGenT m) where
    askRq = XMLGenT askRq
    localRq f (XMLGenT m) = XMLGenT (localRq f m)

instance (FilterMonad a m) => FilterMonad a (XMLGenT m) where
    setFilter = XMLGenT . setFilter
    composeFilter f = XMLGenT (composeFilter f)
    getFilter (XMLGenT m) = XMLGenT (getFilter m)

instance (WebMonad a m) => WebMonad a (XMLGenT m) where
    finishWith r = XMLGenT $ finishWith r

instance (HasRqData m) => (HasRqData (XMLGenT m)) where
    askRqEnv = XMLGenT askRqEnv
    localRqEnv f (XMLGenT m) = XMLGenT (localRqEnv f m)
    rqDataError = XMLGenT . rqDataError

instance (Alternative m, MonadPlus m, Functor m, MonadIO m, ServerMonad m, FilterMonad a m, WebMonad a m, HasRqData m, a ~ Response) => Happstack (XMLGenT m)