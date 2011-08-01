module Happstack.Plugins.Dynamic 
    ( PluginHandle(..)
    , initPlugins
    ) where

import Control.Concurrent.MVar                (newMVar)
import qualified Data.Map        as Map
import Happstack.Plugins.Plugins (PluginHandle(..),initPersistentINotify)

-- |initialize the plugin system and return a 'PluginHandle'
initPlugins :: IO PluginHandle
initPlugins =
    do inotify <- initPersistentINotify
       objMap <- newMVar Map.empty
       return (PluginHandle (inotify, objMap))
