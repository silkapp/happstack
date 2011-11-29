{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
module Happstack.Server.Plugins.Dynamic
    ( PluginHandle
    , initPlugins
    , withMonadIO
    , withMonadIO_
    ) where

import Control.Monad.Trans        (MonadIO(liftIO))
import Language.Haskell.TH        (ExpQ, appE, varE)
import Language.Haskell.TH.Syntax (Name)
import Happstack.Plugins.Dynamic  (initPlugins)
import Happstack.Plugins.LiftName (liftName)
import Happstack.Plugins.Plugins  (PluginHandle, funcTH)


-- |  Dynamically load the specified symbol pass it as an argument to
-- the supplied server monad function.
--
-- This is a wrapper aronud 'withMonadIO_' which ensures the first
-- and second argument stay in-sync.
-- 
-- Usage:
--
-- > $(withMonadIO 'symbol) pluginHandle ghc_args (error "symbol could not be loaded") $ \previous_errors a -> ...
--
withMonadIO :: Name -> ExpQ
withMonadIO name = appE (appE [| withMonadIO_ |] (liftName name)) (varE name)

-- | dynamically load the specified symbol pass it as an argument to
-- the supplied server monad function.
--
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
withMonadIO_ name _ ph ghc_args notloaded use = do
       (errs,ma) <- liftIO $ funcTH ph name ghc_args
       maybe (notloaded errs) (use errs) ma




