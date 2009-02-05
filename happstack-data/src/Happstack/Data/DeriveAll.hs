
{-# LANGUAGE TemplateHaskell, CPP #-}
{-# OPTIONS_GHC -Wall -Werror #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Happstack.Data.DeriveAll
-- Copyright   :  (c) 2009 Happstack.com; (c) 2007 HAppS LLC
-- License     :  BSD3
--
-- Maintainer  :  happs@googlegroups.com
-- Stability   :  experimental
-- Portability :  Not portable
--
-- Concisely specify which classes to derive for your datatypes.
-- As well as the standard derivable classes, it can also
-- derive syb-with-class's 'New.Data' class and Happstack.Data.Default's
-- 'Default' class.
--
-----------------------------------------------------------------------------

module Happstack.Data.DeriveAll (deriveAll, deriveNewData, deriveNewDataNoDefault)
    where

import qualified Data.Generics as Old
import Data.Generics.SYB.WithClass.Derive
import Data.List
import Happstack.Data.Default
import Language.Haskell.TH

deriveNewData :: [Name] -> Q [Dec]
deriveNewData names
 = do nd <- deriveData names
      defaults <- mapM mkDefaultInstance names
      return (nd ++ concat defaults)

deriveNewDataNoDefault :: [Name] -> Q [Dec]
deriveNewDataNoDefault = deriveData

mkDefaultInstance :: Name -> Q [Dec]
mkDefaultInstance name
 = do info <- reify name
      case info of
          TyConI (NewtypeD _ nm tvs _ _) -> return $ deriveDefault True tvs nm
          TyConI (DataD    _ nm tvs _ _) -> return $ deriveDefault True tvs nm
          _ -> fail ("mkDefaultInstance: Bad info: " ++ pprint info)

-- | The 'deriveAll' function takes a list of classes to derive and
-- a block of declarations. It will additionally derive instances for
-- 'Typeable', 'Old.Data' and 'New.Data'.
--
-- Example:
--
-- > $( deriveAll [''Show, ''Eq, ''Default] [d|
-- >        data Foo a = Foo a
-- >        data Bar = Baz | Quux
-- >  |] )
deriveAll :: [Name] -> Q [Dec] -> Q [Dec]
deriveAll classes0 qdecs
 = do decs <- qdecs
      derivedDecs <- deriveDec (filter isDataOrNewtype decs)
      let (classDefault, classes1) = partition (''Default ==) classes0
          classes2 = ''Old.Data : classes1
          addDefaultInstance = not $ null classDefault
          f = addDerivedClasses addDefaultInstance classes2
          decs' = concatMap f decs
      return (decs' ++ derivedDecs)

addDerivedClasses :: Bool -> [Name] -> Dec -> [Dec]
addDerivedClasses def cs (DataD ctxt nm tvs cons derivs)
    = DataD ctxt nm tvs cons (cs ++ derivs)
    : deriveDefault def tvs nm
addDerivedClasses def cs (NewtypeD ctxt nm tvs con derivs)
    = NewtypeD ctxt nm tvs con (cs ++ derivs)
    : deriveDefault def tvs nm
addDerivedClasses _ _ d = [d]

deriveDefault :: Bool -> [Name] -> Name -> [Dec]
deriveDefault False _ _ = []
deriveDefault True tvs n = [InstanceD context instanceHead []]
    where tvs' = map VarT tvs
          mkDef x = ConT ''Default `AppT` x
          context = map mkDef tvs'
          instanceHead = mkDef $ foldl AppT (ConT n) tvs'

isDataOrNewtype :: Dec -> Bool
isDataOrNewtype (DataD {}) = True
isDataOrNewtype (NewtypeD {}) = True
isDataOrNewtype _ = False

