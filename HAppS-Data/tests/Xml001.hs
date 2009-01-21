
{-# OPTIONS_GHC -fth -fglasgow-exts
                -fallow-undecidable-instances
                -fallow-overlapping-instances
                #-}

module Main (main) where

import Control.Monad.Identity
import Data.Generics.SYB.WithClass.Basics
import Data.Maybe
import HAppS.Data

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

flexibleTests :: [Maybe Res]
flexibleTests =
 [mkFTest [Elem "foo" [Elem "bar" [Elem "zap" []]]] (Foo $ Just $ Bar Zap)
 ,mkFTest [            Elem "bar" [Elem "zap" []] ] DefFoo
 ,mkFTest [Elem "foo" [            Elem "zap" [] ]] (Foo Nothing)
 ,mkFTest [Elem "foo" [Elem "bar" [             ]]] (Foo $ Just $ Bar Zip)
 ,mkFTest [Elem "foo" [                          ]] (Foo Nothing)
 ,mkFTest [            Elem "bar" []              ] DefFoo
 ,mkFTest [                        Elem "zap" []  ] DefFoo
 ]

flexibleManualTests :: [Maybe (MyList YesNo)]
flexibleManualTests =
 [mkFTest [] (MkMyList [])
 ,mkFTest [Elem "yes" []] (MkMyList [Yes])
 ,mkFTest [Elem "no"  []] (MkMyList [No])
 ,mkFTest [Elem "yes" [], Elem "yes" []] (MkMyList [Yes, Yes])
 ,mkFTest [Elem "yes" [], Elem "no"  []] (MkMyList [Yes, No])
 ,mkFTest [Elem "no"  [], Elem "yes" []] (MkMyList [No,  Yes])
 ,mkFTest [Elem "no"  [], Elem "no"  []] (MkMyList [No,  No])
 ]

migrationTests :: [Maybe New]
migrationTests =
 [mkFTest [Elem "new" [testtype, newver, CData "5"       ]] (New 5)
 ,mkFTest [Elem "old" [testtype, oldver, Elem  "yes"   []]] (New 9)
 ,mkFTest [Elem "old" [testtype, oldver, Elem  "no"    []]] (New 8)
 ,mkFTest [Elem "new" [                  CData "5"       ]] (New 5)
 ,mkFTest [Elem "new" [                  Elem  "yes"   []]] (New 0)
 ,mkFTest [Elem "new" [                                  ]] (New 0)
 ,mkFTest [Elem "old" [testtype                          ]] (New 7)
 ,mkFTest [Elem "old" [testtype, oldver                  ]] (New 8)
 ,mkFTest [Elem "old" [                                  ]] (New 7)
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

main :: IO ()
main = do mapM_ print flexibleTests
          putStrLn "====="
          mapM_ print flexibleManualTests
          putStrLn "====="
          mapM_ print migrationTests

