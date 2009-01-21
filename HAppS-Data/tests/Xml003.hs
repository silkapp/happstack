
{-# OPTIONS_GHC -fth -fglasgow-exts
                -fallow-undecidable-instances
                -fallow-overlapping-instances #-}

module Main (main) where

import Control.Monad.Identity
import Data.Generics.SYB.WithClass.Basics
import Data.Maybe
import HAppS.Data

$( deriveAll [''Eq, ''Show]
    [d|
        data Foo a = DefFoo | Foo a
        data Bar a = DefBar | Bar a
      |]
 )

instance Default a => Default (Foo a) where
    defaultValue = DefFoo

instance Default a => Default (Bar a) where
    defaultValue = DefBar

testPairs :: Maybe [Foo (Bar String)]
testPairs = let xs = [Foo $ Bar "abc",Foo $ Bar "def"]
                xs' = runIdentity $ fromXml Flexible $ pairsToXml $ xmlToPairs $ concatMap toXml xs
            in if xs == xs' then Nothing else Just xs'

main :: IO ()
main = print testPairs

