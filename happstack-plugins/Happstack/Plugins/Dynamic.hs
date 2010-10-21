module Happstack.Plugins.Dynamic 
    ( PluginHandle(..)
    , initPlugins
    ) where

import Data.IORef                (newIORef)
import qualified Data.Map        as Map
import Happstack.Plugins.Plugins (PluginHandle(..))
import System.INotify            (initINotify)

-- |initialize the plugin system and return a 'PluginHandle'
initPlugins :: IO PluginHandle
initPlugins =
    do inotify <- initINotify
       objMap <- newIORef Map.empty
       return (PluginHandle (inotify, objMap))
