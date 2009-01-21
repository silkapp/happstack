
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

mkRTest :: (Eq a, Xml a) => [Element] -> Maybe a -> Maybe (Maybe a)
mkRTest es v = case fromXml Rigid es of
                   v' | v == v'   -> Nothing
                      | otherwise -> Just v'

main :: IO ()
main = do mapM_ print rigidTests
          mapM_ print rigidManualTests

