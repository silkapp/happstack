{-# LANGUAGE CPP, TemplateHaskell #-}
module Main where

import Control.Monad     (msum)
import Happstack.Server  
#ifdef PLUGINS
import Happstack.Server.Plugins.Dynamic
#else
import Happstack.Server.Plugins.Static
#endif

import Types
import HomePage

main :: IO ()
main =
    do ph <- initPlugins
       simpleHTTP nullConf $ pages ph

pages :: PluginHandle -> ServerPart Response
pages ph =
    msum [ $(withServerPart 'greetingPage) ph (\c -> c { pcWhenCompiled = \_ errs -> mapM_ putStrLn errs }) $
             \_errs greetingPage -> (greetingPage (Greeting "hello") "world")
         ]
