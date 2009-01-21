
{-# LANGUAGE TemplateHaskell, OverlappingInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -fglasgow-exts #-}

-- Check that the SYBWC Data instance for IxSet works, by testing
-- that going to and from XML works.

module Main (main) where

import Control.Monad
import Data.Generics.SYB.WithClass.Basics
import Data.Maybe
import HAppS.Data
import HAppS.Data.IxSet
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)

$( deriveAll [''Eq, ''Ord, ''Default, ''Show]
    [d|
        data Test = Test String Int
      |]
 )

instance Indexable Test String where
    empty =  IxSet [Ix (Map.empty :: Map String (Set Test)),
                    Ix (Map.empty :: Map Int (Set Test))]
    calcs (Test s _) = s ++ "bar"

t1, t2, t3 :: Test
t1 = Test "foo" 2
t2 = Test "foo" 3
t3 = Test "goo" 3

ts :: [Test]
ts = [t1, t2, t3]

ixset :: IxSet Test
ixset = fromList ts

xml :: [Element]
xml = toXml ixset

ixset' :: IxSet Test
ixset' = fromXml xml

ts' :: [Test]
ts' = toList ixset'

verbose :: Bool
verbose = False

main :: IO ()
main = do when verbose $ print ts
          when verbose $ print ixset
          when verbose $ print xml
          when verbose $ print ts'
          if ts == ts'
              then putStrLn "Test passed"
              else putStrLn "Test failed"

