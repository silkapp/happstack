{-# LANGUAGE TemplateHaskell, MagicHash #-}
module Happstack.Plugins.LiftName where

import GHC.Exts
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Syntax.Internals

liftName :: Name -> ExpQ
liftName (Name occName nameFlavour) = appE (appE [| Name |] (liftOccName occName)) (liftNameFlavour nameFlavour)

liftOccName :: OccName -> ExpQ
liftOccName (OccName str) = [| OccName str |]

liftNameSpace :: NameSpace -> ExpQ
liftNameSpace VarName   = [| VarName |]
liftNameSpace DataName  = [| DataName |]
liftNameSpace TcClsName = [| TcClsName |]

liftPkgName :: PkgName -> ExpQ
liftPkgName (PkgName str) = [| PkgName str |]

liftModName :: ModName -> ExpQ
liftModName (ModName str) = [| ModName str |]

liftNameFlavour :: NameFlavour -> ExpQ
liftNameFlavour NameS           = [| NameS |]
liftNameFlavour (NameQ modName) = appE [| NameQ |] (liftModName modName)
liftNameFlavour (NameU i)       = [| case $( lift (I# i) ) of
                                       I# i' -> NameU i' |]
liftNameFlavour (NameL i)       = [| case $( lift (I# i) ) of
                                       I# i' -> NameL i' |]
liftNameFlavour (NameG nameSpace pkgName modName) 
                                = appE (appE (appE [| NameG |] (liftNameSpace nameSpace)) (liftPkgName pkgName)) (liftModName modName)
