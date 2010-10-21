module Happstack.Plugins.Static 
    ( PluginHandle(..) 
    , initPlugins
    ) where

data PluginHandle = PluginHandle

-- | pretend to initialize the plugin system. This is provided so that
-- you can easily switch between dynamic and static mode by only
-- changing the imports via a CPP flag.
initPlugins :: IO PluginHandle
initPlugins = return PluginHandle
