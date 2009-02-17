{-# LANGUAGE TemplateHaskell, FlexibleInstances,
             UndecidableInstances, OverlappingInstances,
             MultiParamTypeClasses, CPP, DeriveDataTypeable #-}
module Happstack.Contrib.HListBase where
import Happstack.Data.DeriveAll
import Happstack.Data.Default
import Data.Typeable

$( deriveAll [''Show,''Default,''Eq,''Read,''Ord]
   [d|
        data Couple a b = Couple a b
        data Nil = Nil
    |]
  )

nil::Nil
nil=Nil
