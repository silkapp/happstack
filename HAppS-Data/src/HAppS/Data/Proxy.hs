{-# LANGUAGE CPP, TemplateHaskell, DeriveDataTypeable #-}
module HAppS.Data.Proxy where

import HAppS.Data.DeriveAll
import HAppS.Data.Default

#ifndef __HADDOCK__
$(deriveAll [''Read,''Show,''Default]
  [d| data Proxy t = Proxy |]
 )
#else
data Proxy t = Proxy deriving (Read, Show, Typeable)
#endif


proxy :: t -> Proxy t
proxy _ = Proxy

unProxy :: Proxy t -> t
unProxy _ = undefined

asProxyType :: t -> Proxy t -> t
asProxyType t _ = t


