{-# LANGUAGE TemplateHaskell #-}
module HAppS.Util.TH where

import Language.Haskell.TH

instanceD' :: CxtQ -> TypeQ -> Q [Dec] -> DecQ
instanceD' ctxt ty decs =
    do decs' <- decs
       let decs'' = filter (not . isSigD) decs'
       instanceD ctxt ty (map return decs'')

isSigD :: Dec -> Bool
isSigD (SigD _ _) = True
isSigD _ = False

