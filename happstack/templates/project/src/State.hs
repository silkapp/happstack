{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators
    #-}
module State where

import Happstack.Data  (Default, Version(..), deriveSerialize, defaultValue, deriveAll)
import Happstack.State ((:+:), Component(..), Dependencies, End, mkMethods)
import State.GuestBook (GuestBook)

-- |top-level application state
-- in this case, the top-level state itself does not contain any state
$(deriveAll [''Show, ''Eq, ''Ord, ''Default]
  [d|
      data AppState = AppState
   |])

$(deriveSerialize ''AppState)
instance Version AppState

-- |top-level application component
-- we depend on the GuestBook component
instance Component AppState where
  type Dependencies AppState = GuestBook :+: End
  initialValue = defaultValue
  
-- create types for event serialization
$(mkMethods ''AppState [])
