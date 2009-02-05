{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, UndecidableInstances, OverlappingInstances #-}
module HAppS.Data.Tests.Xml003 (xml003, testPairs) where

import Control.Monad.Identity
import Data.Generics.SYB.WithClass.Basics
import Data.Maybe
import HAppS.Data
import Test.HUnit(Test(..),(@=?),(~:), assertFailure)

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

-- NOTE: I am not possible the test condition is correct, I am just guessing based on what was there
testPairs :: Test
testPairs = let xs = [Foo $ Bar "abc",Foo $ Bar "def"]
                xs' = runIdentity $ fromXml Flexible $ pairsToXml $ xmlToPairs $ concatMap toXml xs
            in "testPairs" ~: xs @=? xs'

xml003 :: Test
xml003 = "xml003" ~: [ testPairs ]
