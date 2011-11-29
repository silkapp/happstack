{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
module Happstack.Server.Plugins.Static 
    ( PluginHandle
    , initPlugins
    , withMonadIO
    , withMonadIO_
    ) where

import Control.Monad.Trans        (MonadIO(..))
import Happstack.Plugins.Static   (PluginHandle, initPlugins) 
import Happstack.Plugins.LiftName (liftName)
import Language.Haskell.TH        (ExpQ, Name, appE, varE)


-- | A template haskell wrapper around 'withMonadIO_'.
-- Usage:
--
-- > $(withMonadIO 'symbol) pluginHandle ghc_args (error "symbol could not be loaded") $ \previous_errors a -> ...
--
withMonadIO :: Name -> ExpQ
withMonadIO name = appE (appE [| withMonadIO_ |] (liftName name)) (varE name)


-- | a static version of 'Happstack.Server.Plugins.Dynamic.withMonadIO_'
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
withMonadIO_ :: (MonadIO m) => 
                   Name         -- ^ name of the symbol to dynamically load
                -> a            -- ^ the symbol (must be the function refered to by the 'Name' argument)
                -> PluginHandle -- ^ Handle to the function reloader
                -> [String]     -- ^ arguments for ghc
                -> ([String] -> m b)        -- ^ function called if the symbol is not loaded ( either because the
				                            --   last recompilation attempt failed or because it is being 
											--   compiled right now by another thread).
                -> ([String] -> a -> m b)   -- ^ function which uses the loaded result, receives also a 
                                            -- list of errors in the last recompilation attempt
                -> m b
withMonadIO_ _name fun _ph _ghc_args _notloaded use = use [] fun

