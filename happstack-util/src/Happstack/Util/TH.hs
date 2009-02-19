{-# LANGUAGE TemplateHaskell #-}
module Happstack.Util.TH where

import Language.Haskell.TH

-- | Version of 'instanceD' that takes in a Q [Dec] instead of a [Q Dec]
-- and filters out signatures from the list of declarations
instanceD' :: CxtQ -> TypeQ -> Q [Dec] -> DecQ
instanceD' ctxt ty decs =
    do decs' <- decs
       let decs'' = filter (not . isSigD) decs'
       instanceD ctxt ty (map return decs'')

-- | Returns true if the Dec matches a SigD constructor
isSigD :: Dec -> Bool
isSigD (SigD _ _) = True
isSigD _ = False

