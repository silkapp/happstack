{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
module Happstack.Server.Plugins.Dynamic
    ( PluginHandle
    , initPlugins
    , withServerPart
    , withServerPart_
    , withServerPart'
    , withServerPart_'
    ) where

import Control.Monad.Trans        (MonadIO(liftIO))
import Language.Haskell.TH        (ExpQ, appE, varE)
import Language.Haskell.TH.Syntax (Name)
import Happstack.Plugins.Dynamic  (initPlugins)
import Happstack.Plugins.LiftName (liftName)
import Happstack.Plugins.Plugins  (PluginHandle, funcTH')
import Happstack.Server           (ServerMonad, FilterMonad, WebMonad, Response, internalServerError, escape, toResponse)

-- |  dynamically load the specified symbol pass it as an argument to
-- the supplied server monad function.
--
-- This is a wrapper aronud 'withServerPart_' which ensures the first
-- and second argument stay in-sync.
-- 
-- Usage:
--
-- > $(withServerPart 'symbol) pluginHandle $ \a -> ...
--
withServerPart :: Name -> ExpQ
withServerPart name = appE (appE [| withServerPart_ |] (liftName name)) (varE name)

withServerPart' :: Name -> ExpQ
withServerPart' name = appE (appE [| withServerPart_' |] (liftName name)) (varE name)


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
                -> (a -> m b)   -- ^ function which uses the loaded result
                -> m b 
withServerPart_ name fun ph use = withServerPart_' name fun ph [] use

withServerPart_' :: (MonadIO m, ServerMonad m, FilterMonad Response m, WebMonad Response m) => 
                   Name         -- ^ name of the symbol to dynamically load
                -> a            -- ^ the symbol (must be the function refered to by the 'Name' argument)
                -> PluginHandle -- ^ Handle to the function reloader
                -> [String]     -- ^ arguments for ghc
                -> (a -> m b)   -- ^ function which uses the loaded result
                -> m b 
withServerPart_' name _fun ph args use =
    do r <- liftIO $ funcTH' ph name args
       case r of
         (Left e)  -> escape $ internalServerError (toResponse (unlines e))
         (Right f) -> use f

