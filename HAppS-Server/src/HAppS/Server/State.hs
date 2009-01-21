{-# LANGUAGE TemplateHaskell #-}
module HAppS.Server.State 
    (
     module HAppS.Server
    ,module HAppS.State
    ,module HAppS.Data
    ,Data,killThread,threadDelay
    ,ask,asks,MonadReader(..)
    ,put,modify
    ,liftM,liftM2
{-    , systemState
    , systemState_
    , ioH -}
    )
    where

import HAppS.Server
import HAppS.State 
import HAppS.Data
import Data.Generics (Data)
import qualified HAppS.State.ComponentTH as C

import Language.Haskell.TH

import Control.Concurrent   ( killThread , threadDelay)
import Control.Monad.Reader ( ask, asks ,MonadReader(..) )
import Control.Monad.State  ( put, modify )
import Control.Monad        ( liftM, liftM2)

{-
$(systemState "main" 'State 'httpimpl ['other,'iofuncs])

simpleHTTP :: [ServerPart IO] -> Conf -> Handler st

main = stdMain $ simpleHTTP (errWrap:http) :*: \() -> IoH other :*: \() -> IoH iofuncs
       :*: (End :: StdPart a)
-}

{-
ioH io = \() -> IoH io

systemState :: String -> Name -> Name -> [Name] -> Q [Dec]
systemState fnName state http ioFuncs
    = systemState_ fnName state $ [| simpleHTTP (errWrap: $(varE http)) |] :
                                  map (\ioF -> [| ioH $(varE ioF) |]) ioFuncs

systemState_ :: String -> Name -> [ExpQ] -> Q [Dec]
systemState_ fnName state handlers
    = do instances <- C.systemState state
         sig <- sigD (mkName fnName) [t| IO () |]
         let endExpr = sigE (conE 'End) (appT (conT ''StdPart) (conT state))
             expr = appE (varE 'stdMain) $ mkList handlers
             mkList [] = endExpr
             mkList (x:xs) = appE (appE (conE '(:*:)) x) (mkList xs)
         fun <- funD (mkName fnName) [clause [] (normalB expr) []]
         return (sig:fun:instances)
-}

