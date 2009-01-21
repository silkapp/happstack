{-# LANGUAGE TemplateHaskell , FlexibleInstances, UndecidableInstances, CPP,
             OverlappingInstances, DeriveDataTypeable, MultiParamTypeClasses #-}

module HAppS.Store.Util 
{--    (
     module Control.Monad.State
    ,module Control.Monad.Reader
    ,byTime,byRevTime,fun0_1,fun0_2
 
) --}
where
import HAppS.Data
import GHC.Conc
import HAppS.State
import Control.Monad.State
import HAppS.Data.IxSet
import HAppS.Data.Atom
import Language.Haskell.TH

import Control.Monad.State 
import Control.Monad.Reader



--interface with State
#ifndef __HADDOCK__
$( deriveAll [''Show,''Default,''Read,''Eq,''Ord]
   [d|
       newtype Context = Context String  --this belongs elsewhere!
       newtype EpochTime = EpochTime Integer       
       data Wrap a = Wrap {unwrap::a}
       |])
#endif
type With st' st a = Ev (StateT st' STM) a -> Ev (StateT st STM) a


byTime::(Typeable a) => IxSet a -> [a]
byTime = concat . map (\(Published t,es)->es) . groupBy
byRevTime = concat . map (\(Published t,es)->es) . rGroupBy
byRevTime::(Typeable a) => IxSet a -> [a]

fun0_1 name fun arg = 
    FunD (mkName name)  
             [Clause [] (NormalB (AppE (VarE $ mkName fun) 
                                           (ConE $ mkName arg))) 
              []
             ]
fun0_2 name fun arg1 arg2 = 
    FunD (mkName name)  
             [Clause [] (NormalB 
                         (AppE (AppE (VarE $ mkName fun) 
                                (ConE $ mkName arg1))
                                (ConE $ mkName arg2)))
              []
             ]
