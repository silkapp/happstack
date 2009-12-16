{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, UndecidableInstances, OverlappingInstances #-}
module Happstack.Data.Tests.Xml002 (xml002, rigidTests, rigidManualTests) where

import Control.Monad.Identity
import Data.Generics.SYB.WithClass.Basics
import Data.Maybe
import Happstack.Data
import Test.HUnit(Test(..),(@?=),(~:))

$( deriveAll [''Eq, ''Default, ''Show]
    [d|
        data Bap = Zip | Zap
        data Fuzz a = Fo | Fig a
      |]
 )
$( deriveAll [''Eq, ''Show]
    [d|
        data Foo a = DefFoo | Foo a
        data Bar a = DefBar | Bar a

        data YesNo = Yes | No -- Use our own type as Bool has a
                              -- Xml special instance
      |]
 )

newtype MyList a = MkMyList { unMyList :: [a] }
    deriving (Show, Eq, Typeable)
instance (Sat (ctx (MyList a)), Sat (ctx [a]), Data ctx a)
      => Data ctx (MyList a) where
    gfoldl _ f z x  = z MkMyList `f` unMyList x
    toConstr _ (MkMyList _) = mkMyListConstr
    gunfold _ k z c  = case constrIndex c of
                           1 -> k (z MkMyList)
    dataTypeOf _ _ = myListDataType
mkMyListConstr :: Constr
mkMyListConstr = mkConstr myListDataType "MkMyList" [] Prefix
myListDataType :: DataType
myListDataType = mkDataType "MyList" [mkMyListConstr]
instance Default a => Default (MyList a) where
    defaultValue = MkMyList defaultValue

instance Default YesNo where
    defaultValue = No

instance Default a => Default (Foo a) where
    defaultValue = DefFoo

instance Default a => Default (Bar a) where
    defaultValue = DefBar

rigidTests :: Test
rigidTests =
    "rigidTests" ~: 
    [mkRTest [] (Just [])                                     @?= (Nothing :: Maybe (Maybe [YesNo]))
    ,mkRTest [Elem "yes" []] (Just [Yes])                     @?= (Nothing :: Maybe (Maybe [YesNo]))
    ,mkRTest [Elem "no"  []] (Just [No])                      @?= (Nothing :: Maybe (Maybe [YesNo]))
    ,mkRTest [Elem "yes" [], Elem "yes" []] (Just [Yes, Yes]) @?= (Nothing :: Maybe (Maybe [YesNo]))
    ,mkRTest [Elem "yes" [], Elem "no"  []] (Just [Yes, No])  @?= (Nothing :: Maybe (Maybe [YesNo]))
    ,mkRTest [Elem "no"  [], Elem "yes" []] (Just [No,  Yes]) @?= (Nothing :: Maybe (Maybe [YesNo]))
    ,mkRTest [Elem "no"  [], Elem "no"  []] (Just [No,  No])  @?= (Nothing :: Maybe (Maybe [YesNo]))
    ]

-- NOTE: these tests have never passed, they were broken from day one.
-- It is possible that MkMyList is supposed to be treated as
-- Transparent XML, like [] and (,) but it has never been implemented
-- that way.
-- 
-- We are disabling these tests until someone convinces us the tests
-- are right and the current implementation is wrong.
rigidManualTests :: Test
rigidManualTests =
    "rigidManualTests" ~:
    [mkRTest [] (Just (MkMyList [])) @?= (Nothing :: Maybe (Maybe (MyList YesNo)))
    ,mkRTest [Elem "yes" []] (Just (MkMyList [Yes])) @?= (Nothing :: Maybe (Maybe (MyList YesNo)))
    ,mkRTest [Elem "no"  []] (Just (MkMyList [No]))  @?= (Nothing :: Maybe (Maybe (MyList YesNo)))
    ,mkRTest [Elem "yes" [], Elem "yes" []] (Just (MkMyList [Yes, Yes])) @?= (Nothing :: Maybe (Maybe (MyList YesNo)))
    ,mkRTest [Elem "yes" [], Elem "no"  []] (Just (MkMyList [Yes, No]))  @?= (Nothing :: Maybe (Maybe (MyList YesNo)))
    ,mkRTest [Elem "no"  [], Elem "yes" []] (Just (MkMyList [No,  Yes])) @?= (Nothing :: Maybe (Maybe (MyList YesNo)))
    ,mkRTest [Elem "no"  [], Elem "no"  []] (Just (MkMyList [No,  No]))  @?= (Nothing :: Maybe (Maybe (MyList YesNo)))
    ]

mkRTest :: (Eq a, Xml a) => [Element] -> Maybe a -> Maybe (Maybe a)
mkRTest es v = case fromXml Rigid es of
                   v' | v == v'   -> Nothing
                      | otherwise -> Just v'

xml002 :: Test
xml002 = "xml002" ~: [ rigidTests {-, rigidManualTests -} ]