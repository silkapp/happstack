{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, DeriveDataTypeable, UndecidableInstances #-}
module HAppS.Data.Proxy where

import HAppS.Data.DeriveAll
import HAppS.Data.Default

$(deriveAll [''Read,''Show,''Default]
  [d| data Proxy t = Proxy |]
 )

proxy :: t -> Proxy t
proxy _ = Proxy

unProxy :: Proxy t -> t
unProxy _ = undefined

asProxyType :: t -> Proxy t -> t
asProxyType t _ = t


