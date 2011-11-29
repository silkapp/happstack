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
import Types

main :: IO ()
main =
    do ph <- initPlugins
       simpleHTTP nullConf $ pages ph

pages :: PluginHandle -> ServerPart Response
pages ph =
    msum [ $(withMonadIO 'greetingPage) ph ["-O2","-ddump-ds"] notLoadedPage $ \errs greetingPage ->
               (greetingPage (Greeting "hello")$ unlines$ "world!":errs)
         ]
  where 
    notLoadedPage :: [String] -> ServerPart Response
    notLoadedPage errs = escape$ internalServerError$ toResponse$ unlines$ 
                       if null errs then ["Plugin not loaded yet."]
                         else errs
