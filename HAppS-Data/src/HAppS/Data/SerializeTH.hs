{-# LANGUAGE TemplateHaskell, CPP #-}
module HAppS.Data.SerializeTH
    ( deriveSerialize
    , deriveSerializeFor
    ) where

import HAppS.Data.Serialize

import Language.Haskell.TH
import Control.Monad
import Data.Binary

data Class = Tagged [(Name, Int)] Cxt [Name]

deriveSerialize :: Name -> Q [Dec]
#ifndef __HADDOCK__
deriveSerialize name
    = do c <- parseInfo name
         case c of
           Tagged cons cxt keys ->
               do let context = [ mkType ''Serialize [varT key] | key <- keys ] ++ map return cxt
                  i <- instanceD (sequence context) (mkType ''Serialize [mkType name (map varT keys)])
                       [ putCopyFn cons
                       , getCopyFn cons
                       ]
                  return [i]
    where putCopyFn cons
              = do inp <- newName "inp"
                   let putCopyBody = appE (varE 'contain) $
                                     caseE (varE inp) $
                                       [ do args <- replicateM nArgs (newName "arg")
                                            let matchCon = conP conName (map varP args)
                                            match matchCon (normalB (putCopyWork args i)) []
                                             | ((conName,nArgs), i) <- zip cons [0..]]
                       putCopyWork args i
                           = doE $ [noBindS [| putWord8 $(litE (integerL i)) |]] ++
                                   [ noBindS [| safePut $(varE arg) |] | arg <- args ]
                   funD 'putCopy [clause [varP inp] (normalB putCopyBody) []]
          getCopyFn cons
              = let getCopyBody = do c <- newName "c"
                                     appE (varE 'contain) $
                                      doE [bindS (varP c) [| getWord8 |]
                                          , noBindS $ caseE (varE c)
                                                        [ do args <- replicateM nArgs (newName "arg")
                                                             match (litP (integerL i)) (normalB $ getCopyWork conName args) []
                                                          | ((conName, nArgs), i) <- zip cons [0..]]
                                          ]
                    getCopyWork conName args
                        = doE $ [ bindS (varP arg) [| safeGet |] | arg <- args ] ++
                                [ noBindS [| return $(foldl appE (conE conName) (map varE args)) |] ]
                in funD 'getCopy [clause [] (normalB getCopyBody) []]
#endif

deriveSerializeFor :: [Name] -> Q [Dec]
deriveSerializeFor names
    = liftM concat $ mapM deriveSerialize names


mkType con = foldl appT (conT con)

parseInfo :: Name -> Q Class
parseInfo name
    = do info <- reify name
         case info of
           TyConI (DataD cxt _ keys cs _)    -> return $ Tagged (map conInfo cs) cxt keys
           TyConI (NewtypeD cxt _ keys con _)-> return $ Tagged [conInfo con] cxt keys
           _                            -> error "Invalid input"
    where conInfo (NormalC name args) = (name, length args)
          conInfo (RecC name args) = (name, length args)
          conInfo (InfixC _ name _) = (name, 2)
          conInfo (ForallC _ _ con) = conInfo con
