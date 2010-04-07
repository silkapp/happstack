{-# LANGUAGE TemplateHaskell, OverlappingInstances, 
             UndecidableInstances, TemplateHaskell #-}
{-# OPTIONS_GHC -fglasgow-exts #-}

-- Check that the SYBWC Data instance for IxSet works, by testing
-- that going to and from XML works.

module Happstack.Data.IxSet.Tests where

import Control.Monad
import Data.Generics.SYB.WithClass.Basics
import Data.Maybe
import Happstack.Data
import Happstack.Data.IxSet
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import Test.HUnit (Test,(~:),(@=?), test)

$( deriveAll [''Eq, ''Ord, ''Default, ''Show]
    [d|
        data Foo = Foo String Int
        data FooX = Foo1 String Int
                  | Foo2 Int

        data NoIdxFoo = NoIdxFoo Int
      |]
 )

$(inferIxSet "FooXs" ''FooX 'noCalcs [ ''Int, ''String])
{-
  inferIxSet without any indexes is currently unsupported
  $(inferIxSet "NoIdxFoos" ''NoIdxFoo 'noCalcs [])
-}

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


default_ixset :: FooXs
default_ixset = defaultValue

{-
default_ixset_no_idx :: NoIdxFoos
default_ixset_no_idx = defaultValue
-}
ixSetCheckMethodsOnDefault = test 
   [ "size is zero" ~: 0 @=? 
     size default_ixset
   , "getOne returns Nothing" ~: 
     Nothing @=? getOne default_ixset
   , "getOneOr returns default" ~: 
     Foo1 "" 44 @=? getOneOr (Foo1 "" 44) default_ixset 
   , "toList returns []" ~: 
     [] @=? toList default_ixset
   ]

foox_a = Foo1 "abc" 10
foox_b = Foo1 "abc" 20
foox_c = Foo2 10
foox_d = Foo2 20
foox_e = Foo2 30

{-
  defaultValue should work in this case
  doesn't
-}
foox_set_abc :: FooXs
foox_set_abc = insert foox_a $ insert foox_b $ insert foox_c $ defaultValue
foox_set_cde :: FooXs
foox_set_cde = insert foox_e $ insert foox_d $ insert foox_c $ defaultValue

ixSetCheckSetMethods = test 
   [ "size abc is 3" ~: 3 @=? 
     size foox_set_abc
   , "size cde is 3" ~: 3 @=? 
     size foox_set_cde
   , "getOne returns Nothing" ~: 
     Nothing @=? getOne foox_set_abc
   , "getOneOr returns default" ~: 
     Foo1 "" 44 @=? getOneOr (Foo1 "" 44) foox_set_abc
   , "toList returns 3 element list" ~: 
     3 @=? length (toList foox_set_abc)
   ]






allTests :: Test
allTests = "happstack-ixset" ~: [ ixSet001
                                , ixSetCheckMethodsOnDefault
                                , ixSetCheckSetMethods
                                ]
