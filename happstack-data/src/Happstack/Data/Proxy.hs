{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, DeriveDataTypeable, UndecidableInstances #-}
module Happstack.Data.Proxy where

import Happstack.Data.DeriveAll
import Happstack.Data.Default

$(deriveAll [''Read,''Show,''Default]
  [d| data Proxy t = Proxy |]
 )

proxy :: t -> Proxy t
proxy _ = Proxy

unProxy :: Proxy t -> t
unProxy _ = undefined

asProxyType :: t -> Proxy t -> t
asProxyType t _ = t


