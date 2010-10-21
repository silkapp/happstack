{-# LANGUAGE CPP, TemplateHaskell #-}
module Main where

import Control.Monad     (msum)
import Happstack.Server  
#ifdef PLUGINS
import Happstack.Server.Plugins.Dynamic
#else
import Happstack.Server.Plugins.Static
#endif

import HomePage

main :: IO ()
main =
    do ph <- initPlugins
       simpleHTTP nullConf $ pages ph

pages :: PluginHandle -> ServerPart Response
pages ph =
    msum [ $(withServerPart 'helloPage) ph $ \helloPage ->
               (helloPage "world")
         ]
