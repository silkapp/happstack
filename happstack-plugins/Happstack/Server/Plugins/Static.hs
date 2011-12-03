{-# LANGUAGE EmptyDataDecls, FlexibleContexts, TemplateHaskell #-}
module Happstack.Server.Plugins.Static 
    ( PluginHandle
    , PluginConf(..)
    , initPlugins
    , withServerPart
    , withServerPart_
    , withServerPart'
    , withServerPart_'
    ) where

import Control.Monad.Trans        (MonadIO(..))
import Happstack.Plugins.Static   (PluginHandle, initPlugins) 
import Happstack.Plugins.LiftName (liftName)
import Happstack.Server           (ServerMonad, FilterMonad, WebMonad, Response)
import Language.Haskell.TH        (ExpQ, Name, appE, varE)


-- | A template haskell wrapper around 'withMonadIO_'.
-- Usage:
--
-- > $(withServerPart 'symbol) pluginHandle $ \a -> ...
--
withMonadIO :: Name -> ExpQ
withMonadIO name = appE (appE [| withMonadIO_ |] (liftName name)) (varE name)

withServerPart' :: Name -> ExpQ
withServerPart' name = appE (appE [| withServerPart_' |] (liftName name)) (varE name)

-- | a static version of 'Happstack.Server.Plugins.Dynamic.withServerPart_'
--
-- This function has the same signature as its dynamic sibling, but it
-- does not do any fancy dynamic loading. It simply applies the
-- function to the supplied value.
--
-- This function exists so that you can that you can compile using
-- dynamic plugins during development, but statically link the final
-- build.
-- 
-- Use a CPP to select between the Dynamic and Static versions of this module.
withServerPart_ :: (MonadIO m, ServerMonad m, FilterMonad Response m, WebMonad Response m) => Name -> a -> PluginHandle -> (a -> m b) -> m b
withServerPart_ _name fun _objMap use = use fun

withServerPart_' :: (MonadIO m, ServerMonad m, FilterMonad Response m, WebMonad Response m) => Name -> a -> PluginHandle -> [String] -> (a -> m b) -> m b
withServerPart_' _name fun _objMap _args use = use fun

