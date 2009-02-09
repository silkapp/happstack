{-# LANGUAGE TemplateHaskell, CPP #-}
module Happstack.Data.SerializeTH
    ( deriveSerialize
    , deriveSerializeFor
    ) where

import Happstack.Data.Serialize

import Language.Haskell.TH
import Control.Monad
import Data.Binary

data Class = Tagged [(Name, Int)] Cxt [Name]

deriveSerialize :: Name -> Q [Dec]
deriveSerialize name
    = do c <- parseInfo name
         case c of
           Tagged cons cx keys ->
               do let context = [ mkType ''Serialize [varT key] | key <- keys ] ++ map return cx
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
                                          , noBindS $ caseE (varE c) $
                                                        [ do args <- replicateM nArgs (newName "arg")
                                                             match (litP (integerL i)) (normalB $ getCopyWork conName args) []
                                                          | ((conName, nArgs), i) <- zip cons [0..]]
                                                        ++ [match (return WildP) (normalB [|error "Wrong serialization type"|]) []]
                                          ]
                    getCopyWork conName args
                        = doE $ [ bindS (varP arg) [| safeGet |] | arg <- args ] ++
                                [ noBindS [| return $(foldl appE (conE conName) (map varE args)) |] ]
                in funD 'getCopy [clause [] (normalB getCopyBody) []]


deriveSerializeFor :: [Name] -> Q [Dec]
deriveSerializeFor = liftM concat . mapM deriveSerialize


mkType :: Name -> [TypeQ] -> TypeQ
mkType con = foldl appT (conT con)

parseInfo :: Name -> Q Class
parseInfo name
    = do info <- reify name
         case info of
           TyConI (DataD cx _ keys cs _)    -> return $ Tagged (map conInfo cs) cx keys
           TyConI (NewtypeD cx _ keys con _)-> return $ Tagged [conInfo con] cx keys
           _                            -> error "Invalid input"
    where conInfo (NormalC n args) = (n, length args)
          conInfo (RecC n args) = (n, length args)
          conInfo (InfixC _ n _) = (n, 2)
          conInfo (ForallC _ _ con) = conInfo con
