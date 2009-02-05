{-# LANGUAGE TemplateHaskell, FlexibleInstances,
             UndecidableInstances, OverlappingInstances,
             MultiParamTypeClasses, CPP, DeriveDataTypeable #-}
module HAppS.Data.HListBase where
import HAppS.Data.DeriveAll
import HAppS.Data.Default
import Data.Typeable

$( deriveAll [''Show,''Default,''Eq,''Read,''Ord]
   [d|
        data Couple a b = Couple a b
        data Nil = Nil
    |]
  )

nil::Nil
nil=Nil
