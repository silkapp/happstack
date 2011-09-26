module Happstack.Plugins.Dynamic 
    ( PluginHandle(..)
    , initPlugins
    ) where

import Control.Concurrent.MVar                (newMVar)
import qualified Data.Map        as Map
import Happstack.Plugins.Plugins (PluginHandle(..))
import Happstack.Plugins.FileSystemWatcher(initFSWatcher)

-- |initialize the plugin system and return a 'PluginHandle'
initPlugins :: IO PluginHandle
initPlugins =
    do inotify <- initFSWatcher
       objMap <- newMVar Map.empty
       return (PluginHandle (inotify, objMap))
