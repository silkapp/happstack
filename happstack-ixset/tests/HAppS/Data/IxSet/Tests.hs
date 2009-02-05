
{-# LANGUAGE TemplateHaskell, OverlappingInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -fglasgow-exts #-}

-- Check that the SYBWC Data instance for IxSet works, by testing
-- that going to and from XML works.

module HAppS.Data.IxSet.Tests (allTests, ixSet001) where

import Control.Monad
import Data.Generics.SYB.WithClass.Basics
import Data.Maybe
import HAppS.Data
import HAppS.Data.IxSet
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import Test.HUnit (Test,(~:),(@=?))

$( deriveAll [''Eq, ''Ord, ''Default, ''Show]
    [d|
        data Foo = Foo String Int
      |]
 )

instance Indexable Foo String where
    empty =  IxSet [Ix (Map.empty :: Map String (Set Foo)),
                    Ix (Map.empty :: Map Int (Set Foo))]
    calcs (Foo s _) = s ++ "bar"

t1, t2, t3 :: Foo
t1 = Foo "foo" 2
t2 = Foo "foo" 3
t3 = Foo "goo" 3

ts :: [Foo]
ts = [t1, t2, t3]

ixset :: IxSet Foo
ixset = fromList ts

xml :: [Element]
xml = toXml ixset

ixset' :: Maybe (IxSet Foo)
ixset' = fromXml Rigid xml

ts' :: Maybe [Foo]
ts' = fmap toList ixset'

ixSet001 :: Test
ixSet001 = "ixSet001" ~: (Just ts) @=? ts'

allTests :: Test
allTests = "happstack-ixset" ~: [ixSet001]
