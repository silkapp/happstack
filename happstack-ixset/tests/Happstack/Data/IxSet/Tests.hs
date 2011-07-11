{-# LANGUAGE TemplateHaskell, OverlappingInstances, 
             UndecidableInstances, TemplateHaskell #-}
{-# OPTIONS_GHC -fglasgow-exts #-}

-- Check that the SYBWC Data instance for IxSet works, by testing
-- that going to and from XML works.

module Happstack.Data.IxSet.Tests where

import Control.Monad
import Happstack.Data
import Happstack.Data.IxSet as IxSet
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import Test.HUnit (Test,(~:),(@=?), test)
import Control.Exception as E
import Test.QuickCheck
import Happstack.Util.Testing
import Data.Data as Data
import Data.Maybe

$( deriveAll [''Eq, ''Ord, ''Default, ''Show]
    [d|
        data Foo = Foo String Int
        data FooX = Foo1 String Int
                  | Foo2 Int

        data NoIdxFoo = NoIdxFoo Int
        data BadlyIndexed = BadlyIndexed Int

        data MultiIndex = MultiIndex String Int Integer (Maybe Int) (Either Bool Char)
                        | MultiIndexSubset Int Bool String
        data Triple = Triple Int Int Int
        data S = S String
        data G a b = G a b
        
      |]
 )

$(inferIxSet "FooXs" ''FooX 'noCalcs [''Int, 
                                      ''String
                                      ])

$(inferIxSet "BadlyIndexeds" ''BadlyIndexed 'noCalcs [''String])

$(inferIxSet "MultiIndexed" ''MultiIndex 'noCalcs [''String, ''Int, ''Integer, ''Bool, ''Char])

$(inferIxSet "Triples" ''Triple 'noCalcs [''Int])

fooCalcs (Foo s _) = s ++ "bar"



$(inferIxSet "Gs" ''G 'noCalcs [''Int])



{-
instance Indexable Foo String where
    empty =  ixSet [ -- Ix (Map.empty :: Map String (Set Foo)) flattenWithCalcs
                     ixGenWithCalcs (undefined :: String)
                   , -- Ix (Map.empty :: Map Int (Set Foo)) flattenWithCalcs
                     ixGenWithCalcs (undefined :: Int)
-}

$(inferIxSet "Foos" ''Foo 'fooCalcs [''String, ''Int])

instance Indexable S where
    empty =  ixSet [ ixFun (\(S x) -> [length x])
                   ]
    -- calcs _ = ()



ixSetCheckMethodsOnDefault :: Test
ixSetCheckMethodsOnDefault = "ixSetCheckMethodsOnDefault" ~: test 
   [ "size is zero" ~: 0 @=? 
     size (defaultValue :: Foos)
   , "getOne returns Nothing" ~: 
     Nothing @=? getOne (defaultValue :: Foos)
   , "getOneOr returns default" ~: 
     Foo1 "" 44 @=? getOneOr (Foo1 "" 44) defaultValue
   , "toList returns []" ~: 
     [] @=? toList (defaultValue :: Foos)
   ]

foox_a :: FooX
foox_a = Foo1 "abc" 10
foox_b :: FooX
foox_b = Foo1 "abc" 20
foox_c :: FooX
foox_c = Foo2 10
foox_d :: FooX
foox_d = Foo2 20
foox_e :: FooX
foox_e = Foo2 30

foox_set_abc :: FooXs
foox_set_abc = insert foox_a $ insert foox_b $ insert foox_c $ defaultValue
foox_set_cde :: FooXs
foox_set_cde = insert foox_e $ insert foox_d $ insert foox_c $ defaultValue

ixSetCheckSetMethods :: Test
ixSetCheckSetMethods = "ixSetCheckSetMethods" ~: test 
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

isError :: a -> IO Bool
isError x = (x `seq` return False) `E.catch` \(ErrorCall _) -> return True

badIndexSafeguard :: Test
badIndexSafeguard = "badIndexSafeguard" ~: test 
                    [ "check if there is error when no first index on value" ~:
                      isError (size $ insert (BadlyIndexed 123) empty)
                    , "check if indexing with missing index" ~: 
                      isError (getOne (foox_set_cde @= True))
                    ]

testTriple :: Test
testTriple = "testTriple" ~: test
             [ "check if we can find element" ~:
               1 @=? size ((insert (Triple 1 2 3) empty) 
                           @= (1::Int) @= (2::Int))
             ]


instance Arbitrary Foo where
    arbitrary = liftM2 Foo arbitrary arbitrary

instance (Arbitrary a,Data.Data a, Ord a, Indexable a) => 
    Arbitrary (IxSet a) where
    arbitrary = liftM fromList arbitrary

prop_toAndFromXml :: Foos -> Bool
prop_toAndFromXml ixset = Just ixset == ixset'
    where
      xml :: [Element]
      xml = toXml ixset

      ixset' :: Maybe (IxSet Foo)
      ixset' = fromXml Rigid xml

prop_sizeEqToListLength :: Foos -> Bool      
prop_sizeEqToListLength ixset = size ixset == length (toList ixset)

prop_union :: Foos -> Foos -> Bool
prop_union ixset1 ixset2 = 
    toSet (ixset1 `union` ixset2) == toSet ixset1 `Set.union` toSet ixset2

prop_intersection :: Foos -> Foos -> Bool
prop_intersection ixset1 ixset2 = 
    toSet (ixset1 `intersection` ixset2) == 
          toSet ixset1 `Set.intersection` toSet ixset2

prop_opers :: Foos -> Int -> Bool
prop_opers ixset intidx =
    ((lt `union` eq) == lteq) &&
    ((gt `union` eq) == gteq) &&
    -- this works for Foo as an Int field is in every Foo value
    ((gt `union` eq `union` lt) == ixset) 
    where
      eq = ixset @= intidx
      lt = ixset @< intidx
      gt = ixset @> intidx
      lteq = ixset @<= intidx
      gteq = ixset @>= intidx

prop_sureelem :: Foos -> Foo -> Bool
prop_sureelem ixset foo@(Foo string intidx) = 
    not (IxSet.null eq) && 
    not (IxSet.null lteq) && 
    not (IxSet.null gteq)
    where
      ixset' = insert foo ixset
      eq = ixset' @= intidx
      lteq = ixset' @<= intidx
      gteq = ixset' @>= intidx

prop_ranges :: Foos -> Int -> Int -> Bool
prop_ranges ixset intidx1 intidx2 =
    ((ixset @>< (intidx1,intidx2)) == (gt1 &&& lt2)) &&
    ((ixset @>=< (intidx1,intidx2)) == ((gt1 ||| eq1) &&& lt2)) &&
    ((ixset @><= (intidx1,intidx2)) == (gt1 &&& (lt2 ||| eq2))) &&
    ((ixset @>=<= (intidx1,intidx2)) == ((gt1 ||| eq1) &&& (lt2 ||| eq2)))
    where
      eq1 = ixset @= intidx1
      lt1 = ixset @< intidx1
      gt1 = ixset @> intidx1
      eq2 = ixset @= intidx2
      lt2 = ixset @< intidx2
      gt2 = ixset @> intidx2
      

prop_any :: Foos -> [Int] -> Bool
prop_any ixset idxs = 
    (ixset @+ idxs) == foldr union empty (map ((@=) ixset) idxs)

prop_all :: Foos -> [Int] -> Bool
prop_all ixset idxs = 
    (ixset @* idxs) == foldr intersection ixset (map ((@=) ixset) idxs)

funSet :: IxSet S
funSet = IxSet.fromList [S "", S "abc", S "def", S "abcde"]

funIndexes :: Test
funIndexes = "funIndexes" ~: test 
   [ "has zero length element" ~: 1 @=? 
     size (funSet @= (0 :: Int))
   , "has two lengh 3 elements" ~: 2 @=? 
     size (funSet @= (3 :: Int))
   , "has three lengh [3;7] elements" ~: 3 @=? 
     size (funSet @>=<= (3 :: Int, 7 :: Int))
   ]

allTests :: Test
allTests = "happstack-ixset" ~: [ ixSetCheckMethodsOnDefault
                                , ixSetCheckSetMethods
                                , badIndexSafeguard
                                , test (True @=? findElement 1 1)
                                , testTriple
                                , funIndexes
                                , "prop_toAndFromXml"       ~: qctest prop_toAndFromXml
                                , "prop_sizeEqToListLength" ~: qctest prop_sizeEqToListLength
                                , "prop_union"              ~: qctest prop_union
                                , "prop_union"              ~: qctest prop_intersection
                                , "prop_opers"              ~: qctest prop_opers
                                , "prop_sureelem"           ~: qctest prop_sureelem
                                , "prop_ranges"             ~: qctest prop_ranges
                                , "prop_any"                ~: qctest prop_any
                                , "prop_all"                ~: qctest prop_all
                                ]



bigSet :: Int -> MultiIndexed
bigSet n = fromList $
    [ MultiIndex string int integer maybe_int either_bool_char |
      string <- ["abc", "def", "ghi", "jkl"],
      int <- [1..n],
      integer <- [10000..10010],
      maybe_int <- [Nothing, Just 5, Just 6],
      either_bool_char <- [Left True, Left False, Right 'A', Right 'B']] ++
    [ MultiIndexSubset int bool string |
      string <- ["abc", "def", "ghi"],
      int <- [1..n],
      bool <- [True, False]]


{-
GHCi

let b = bigSet 100
to evaluate things
size b

timed $ findElementX b 1

2010-05-09 6:00 daje 50s

-}

findElementX :: MultiIndexed -> Int -> Bool
findElementX set n = isJust $ getOne (set @+ ["abc","def","ghi"] 
                                      @>=<= (10000 :: Integer,10010 :: Integer)
                                      @= (True :: Bool)
                                      @= (n `div` n)
                                      @= "abc"
                                      @= (10000 :: Integer)
                                      @= (5 :: Int))

findElement :: Int -> Int -> Bool
findElement n m = all id ([findElementX set k | k <- [1..n]])
    where set = bigSet m
                            
{-
bigSetTests :: Test
bigSetTests = "bigSetTests" ~: return (findElement 1 1)
-}
