
{-# OPTIONS_GHC -fth -fglasgow-exts
                -fallow-undecidable-instances
                -fallow-overlapping-instances #-}

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

rigidTests :: [Maybe (Maybe [YesNo])]
rigidTests =
 [mkRTest [] (Just [])
 ,mkRTest [Elem "yes" []] (Just [Yes])
 ,mkRTest [Elem "no"  []] (Just [No])
 ,mkRTest [Elem "yes" [], Elem "yes" []] (Just [Yes, Yes])
 ,mkRTest [Elem "yes" [], Elem "no"  []] (Just [Yes, No])
 ,mkRTest [Elem "no"  [], Elem "yes" []] (Just [No,  Yes])
 ,mkRTest [Elem "no"  [], Elem "no"  []] (Just [No,  No])
 ]

rigidManualTests :: [Maybe (Maybe (MyList YesNo))]
rigidManualTests =
 [mkRTest [] (Just (MkMyList []))
 ,mkRTest [Elem "yes" []] (Just (MkMyList [Yes]))
 ,mkRTest [Elem "no"  []] (Just (MkMyList [No]))
 ,mkRTest [Elem "yes" [], Elem "yes" []] (Just (MkMyList [Yes, Yes]))
 ,mkRTest [Elem "yes" [], Elem "no"  []] (Just (MkMyList [Yes, No]))
 ,mkRTest [Elem "no"  [], Elem "yes" []] (Just (MkMyList [No,  Yes]))
 ,mkRTest [Elem "no"  [], Elem "no"  []] (Just (MkMyList [No,  No]))
 ]

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

mkRTest :: (Eq a, Xml a) => [Element] -> Maybe a -> Maybe (Maybe a)
mkRTest es v = case fromXml Rigid es of
                   v' | v == v'   -> Nothing
                      | otherwise -> Just v'

testPairs :: Maybe [Foo (Bar String)]
testPairs = let xs = [Foo $ Bar "abc",Foo $ Bar "def"]
                xs' = runIdentity $ fromXml Flexible $ pairsToXml $ xmlToPairs $ concatMap toXml xs
            in if xs == xs' then Nothing else Just xs'

main :: IO ()
main = do runTests flexibleTests
          runTests flexibleManualTests
          runTests migrationTests
          runTests rigidTests
          runTests rigidManualTests
          let resPairs = testPairs
          if isNothing resPairs
              then putStrLn "Pair test passed"
              else putStrLn ("Pair test failed: " ++ show resPairs)

runTests :: Show a => [Maybe a] -> IO ()
runTests tests = if all isNothing tests
                 then putStrLn "Tests passed"
                 else putStrLn ("Tests failed: " ++ show tests)

-------EXAMPLE of Hlist
$( deriveAll [''Show,''Eq, ''Default]
   [d|
       data UserInfo = UserInfo User Pass 
       newtype User = User String 
       newtype Pass = Pass String 
       newtype Age = Age Int
    |]
 )

tHasT::HasT hlist y => hlist -> y -> Pairs
tHasT hlist = error "a"
t8 = tHasT (User "alex" .&. "asad") (User "alex")
t9 = tHasT (User "alex" .&. "asad" ) "abc" 
-- e9 = tHasT (User "alex" .&. "asad" )  1233 -- compiler error!

{-
t10 = extract (User "alex" .&. "abc") == "abc"
t10_2 = extract (User "alex" .&. Pass "pass" .&. "abc") == "abc"
--- e10 = extract (User "alex" .&. "abc") == 2 -- should be compiler error and is
t11 = Pass "pass2" == (extract $ update (User "alex" .&. Pass "pass") (Pass "pass2") )

t12 = (User "ales" .&. Pass "pass" .&. Age 55 )
t13 = toPairs t12
t14 = fromPairs t13 
t15 = t14 == t12
-}
