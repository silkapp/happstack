{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, DeriveDataTypeable, UndecidableInstances #-}
module Happstack.Data.Proxy where

import Happstack.Data.DeriveAll
import Happstack.Data.Default

-- | Proxy is empty and is used as a transport of a phantom type
$(deriveAll [''Read,''Show,''Default]
  [d| data Proxy t = Proxy |]
 )

-- | Creates the Proxy with a type matching the argument
proxy :: t -> Proxy t
proxy _ = Proxy

-- | Returns bottom
unProxy :: Proxy t -> t
unProxy _ = undefined

-- | Acts as id except for providing type restrictions
asProxyType :: t -> Proxy t -> t
asProxyType t _ = t


