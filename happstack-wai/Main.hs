{-# LANGUAGE FunctionalDependencies, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeSynonymInstances, RankNTypes, ScopedTypeVariables #-}
module Main where

import Control.Monad
import Happstack.Server.Guards
import Happstack.Server.Monads
import Happstack.Server.Filters
import Happstack.Server.ToMessage
import Happstack.Server.Simple

main :: IO ()
main = simpleHTTP 8000 $ 
       msum [ dir "foo" $ ok $ toResponse "foo" -- handles /foo
            , dir "bar" $ ok $ toResponse "bar" -- handles /bar
            , do nullDir                        -- handles /
                 ok $ toResponse "hello"
            , notFound $ toResponse "Invalid URL" -- handles anything else
            ]
