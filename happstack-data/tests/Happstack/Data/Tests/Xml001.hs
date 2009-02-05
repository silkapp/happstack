{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances, DeriveDataTypeable #-}
module Happstack.Data.Tests.Xml001 (xml001, flexibleTests, flexibleManualTests, migrationTests) where

import Control.Monad.Identity
import Data.Generics.SYB.WithClass.Basics
import Data.Maybe
import Happstack.Data
import Test.HUnit (Test(..),(@?=),(~:))

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

        data New = New Int
        data Old = Old YesNo
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
                           _ -> error "gunfold MyList: Can't happen"
    dataTypeOf _ _ = myListDataType
mkMyListConstr :: Constr
mkMyListConstr = mkConstr myListDataType "MkMyList" [] Prefix
myListDataType :: DataType
myListDataType = mkDataType "MyList" [mkMyListConstr]
instance Default a => Default (MyList a) where
    defaultValue = MkMyList defaultValue

instance Default New where
    defaultValue = New 7

instance Default Old where
    defaultValue = Old Yes

instance Xml New where
    version _ = Just "newver"
    otherVersion _ = Other (error "Other" :: Old)

instance Xml Old where
    typ _ = "New"

instance Migrate Old New where
    migrate (Old No)  = New 8
    migrate (Old Yes) = New 9

instance Default YesNo where
    defaultValue = No

instance Default a => Default (Foo a) where
    defaultValue = DefFoo

instance Default a => Default (Bar a) where
    defaultValue = DefBar

flexibleTests :: Test
flexibleTests =
    "flexibleTests" ~:
 [mkFTest [Elem "foo" [Elem "bar" [Elem "zap" []]]] (Foo $ Just $ Bar Zap)  @?= (Nothing :: Maybe Res)
 ,mkFTest [            Elem "bar" [Elem "zap" []] ] DefFoo                  @?= (Nothing :: Maybe Res)
 ,mkFTest [Elem "foo" [            Elem "zap" [] ]] (Foo Nothing)           @?= (Nothing :: Maybe Res)
 ,mkFTest [Elem "foo" [Elem "bar" [             ]]] (Foo $ Just $ Bar Zip)  @?= (Nothing :: Maybe Res)
 ,mkFTest [Elem "foo" [                          ]] (Foo Nothing)           @?= (Nothing :: Maybe Res)
 ,mkFTest [            Elem "bar" []              ] DefFoo                  @?= (Nothing :: Maybe Res)
 ,mkFTest [                        Elem "zap" []  ] DefFoo                  @?= (Nothing :: Maybe Res)
 ]

flexibleManualTests :: Test
flexibleManualTests =
    "flexibleManualTest" ~:
 [mkFTest [] (MkMyList [])                 @?= (Nothing :: Maybe (MyList YesNo))
 ,mkFTest [Elem "yes" []] (MkMyList [Yes]) @?= (Nothing :: Maybe (MyList YesNo))
 ,mkFTest [Elem "no"  []] (MkMyList [No])  @?= (Nothing :: Maybe (MyList YesNo))
 ,mkFTest [Elem "yes" [], Elem "yes" []] (MkMyList [Yes, Yes]) @?= (Nothing :: Maybe (MyList YesNo))
 ,mkFTest [Elem "yes" [], Elem "no"  []] (MkMyList [Yes, No])  @?= (Nothing :: Maybe (MyList YesNo))
 ,mkFTest [Elem "no"  [], Elem "yes" []] (MkMyList [No,  Yes]) @?= (Nothing :: Maybe (MyList YesNo))
 ,mkFTest [Elem "no"  [], Elem "no"  []] (MkMyList [No,  No])  @?= (Nothing :: Maybe (MyList YesNo))
 ]

migrationTests :: Test
migrationTests =
 "migrationTests" ~:
 [mkFTest [Elem "new" [testtype, newver, CData "5"       ]] (New 5) @?= Nothing
 ,mkFTest [Elem "old" [testtype, oldver, Elem  "yes"   []]] (New 9) @?= Nothing
 ,mkFTest [Elem "old" [testtype, oldver, Elem  "no"    []]] (New 8) @?= Nothing
 ,mkFTest [Elem "new" [                  CData "5"       ]] (New 5) @?= Nothing
 ,mkFTest [Elem "new" [                  Elem  "yes"   []]] (New 0) @?= Nothing
 ,mkFTest [Elem "new" [                                  ]] (New 0) @?= Nothing
 ,mkFTest [Elem "old" [testtype                          ]] (New 7) @?= Nothing
 ,mkFTest [Elem "old" [testtype, oldver                  ]] (New 8) @?= Nothing
 ,mkFTest [Elem "old" [                                  ]] (New 7) @?= Nothing
 ]

newver :: Element
newver = Attr versionAttr "newver"

oldver :: Element
oldver = Attr versionAttr "oldver"

testtype :: Element
testtype = Attr typeAttr (dataTypeName (dataTypeOf xmlProxy (undefined :: New)))

type Res = Foo (Maybe (Bar Bap))

mkFTest :: (Eq a, Xml a) => [Element] -> a -> Maybe a
mkFTest es v = case fromXml Flexible es of
                   Identity v' | v == v'   -> Nothing
                               | otherwise -> Just v'

xml001 :: Test
xml001 = "xml001" ~: [ flexibleTests, flexibleManualTests, migrationTests ]