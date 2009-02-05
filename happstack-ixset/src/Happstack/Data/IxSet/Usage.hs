{-# LANGUAGE UndecidableInstances, OverlappingInstances, FlexibleInstances
             ,MultiParamTypeClasses, TemplateHaskell, DeriveDataTypeable,
              TypeSynonymInstances #-}

-- XXX This modules should be turned into documentation and/or test cases

module Happstack.Data.IxSet.Usage where

import Happstack.Data.IxSet

import Data.Generics hiding (GT)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)

{--
HOW TO USE
--}

--define your datatype
data Test = Test String Int deriving (Data,Typeable,Eq,Ord,Show,Read)

--define the db on the datatype
--should be able to do template haskell to generate these


instance Indexable Test String where
    empty =  IxSet [Ix (Map.empty :: Map String (Set Test)),
                    Ix (Map.empty :: Map Int (Set Test))]
    calcs (Test s _) = s ++ "bar"

t1, t2, t3 :: Test
t1 = Test "foo" 2
t2 = Test "foo" 3
t3 = Test "goo" 3

c1, c2, c3, c4 :: IxSet Test
c1 = insert t1 empty
c2 = insert t2 c1
c3 = delete t1 c2
c4 = insert t3 c2

s1, s2, s3, s4, s5, s6, s7, s8 :: IxSet Test
s1 = getEQ "foo" c4
s2 = getEQ (3::Int) c4
s3 = getLT (4::Int) c4
s4 = getGT (2::Int) c4
s5 = getRange "foo" "goo" c4
s6 = getLT (4::Int) s5
s7 = union s6 s4
s8 = getEQ "foobar" c4

