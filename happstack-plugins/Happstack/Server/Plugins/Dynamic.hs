{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
module Happstack.Server.Plugins.Dynamic
    ( PluginHandle
    , PluginConf(..)
    , initPlugins
    , initPluginsWithConf
    , defaultPluginConf
    , withServerPart
    , withServerPart_
    ) where

import Control.Monad.Trans          (MonadIO)
import Language.Haskell.TH          (ExpQ, Name, appE, varE)
import Language.Haskell.TH.Lift     (lift)
import System.Plugins.Auto          ( initPlugins,initPluginsWithConf,PluginHandle,withMonadIO_
                                    , PluginConf(..), defaultPluginConf)
import Happstack.Server             (ServerMonad, FilterMonad, WebMonad, Response, internalServerError, escape, toResponse)

-- |  dynamically load the specified symbol pass it as an argument to
-- the supplied server monad function.
--
-- This is a wrapper aronud 'withServerPart_' which ensures the first
-- and second argument stay in-sync.
-- 
-- Usage:
--
-- > $(withServerPart 'symbol) pluginHandle id $ \errors a -> ...
--
withServerPart :: Name -> ExpQ
withServerPart name = appE (appE [| withServerPart_ |] (lift name)) (varE name)

-- | dynamically load the specified symbol pass it as an argument to
-- the supplied server monad function.
--
-- If something fails, this function will return '500 Internal Server
-- Error' and a list of the errors encountered.
--
-- see also: 'withServerPart'
withServerPart_ :: (MonadIO m, ServerMonad m, FilterMonad Response m, WebMonad Response m) => 
                   Name         -- ^ name of the symbol to dynamically load
                -> a            -- ^ the symbol (must be the function refered to by the 'Name' argument)
                -> PluginHandle -- ^ Handle to the function reloader
                -> ([String] -> a -> m b)   -- ^ function which uses the loaded result, and gets a list of compilation errors if any
                -> m b 
withServerPart_ name fun ph use = withMonadIO_ name fun ph notLoaded use
 where
   notLoaded errs = escape $ internalServerError$ toResponse$ 
       case errs of
         [] -> "Module not loaded yet."
         _ -> unlines errs

