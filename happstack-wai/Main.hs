{-# LANGUAGE FunctionalDependencies, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeSynonymInstances, RankNTypes, ScopedTypeVariables #-}
module Main where

import Control.Monad
import Happstack.Server.Guards
import Happstack.Server.ToMessage
import Happstack.Server.Simple

main :: IO ()
main = simpleHTTP 8000 $ 
       msum [ dir "foo" $ return $ toResponse "foo"
            , dir "bar" $ return $ toResponse "bar"
            , return $ toResponse "hello"
            ]
