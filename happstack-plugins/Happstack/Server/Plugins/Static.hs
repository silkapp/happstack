{-# LANGUAGE EmptyDataDecls, FlexibleContexts, TemplateHaskell #-}
module Happstack.Server.Plugins.Static 
    ( PluginHandle
    , PluginConf(..)
    , initPlugins
    , initPluginsWithConf
    , defaultPluginConf
    , withServerPart
    , withServerPart_
    ) where

import Control.Monad.Trans          (MonadIO)
import Happstack.Server             (ServerMonad, FilterMonad, WebMonad, Response)
import Language.Haskell.TH          (ExpQ, Name, appE, varE)
import System.Plugins.Auto.LiftName
import System.Plugins.Auto          (PluginConf(..),defaultPluginConf)

-- | A template haskell wrapper around 'withServerPart_'.
-- Usage:
--
-- > $(withServerPart 'symbol) pluginHandle id $ \errors a -> ...
--
withServerPart :: Name -> ExpQ
withServerPart name = appE (appE [| withServerPart_ |] (liftName name)) (varE name)

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
withServerPart_ :: (MonadIO m, ServerMonad m, FilterMonad Response m, WebMonad Response m) => Name -> a -> PluginHandle -> (PluginConf -> PluginConf) -> ([String] -> a -> m b) -> m b
withServerPart_ _name fun _objMap _args use = use [] fun

-- | Dummy plugin handle. In a static configuration its values are not used at all.
data PluginHandle

-- | Creates a dummy plugin handle.
initPlugins :: IO PluginHandle
initPlugins = initPluginsWithConf undefined

-- | Creates a dummy plugin handle.
initPluginsWithConf :: PluginConf -> IO PluginHandle
initPluginsWithConf = const$ return undefined

