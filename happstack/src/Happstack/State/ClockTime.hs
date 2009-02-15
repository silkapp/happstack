{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, StandaloneDeriving, TypeSynonymInstances
    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |instances of Typeable, Data, Serialize, Version, and Default for ClockTime
module Happstack.State.ClockTime (ClockTime(..)) where

import Data.Generics (Data, Typeable)
import Happstack.Data (deriveNewData)
import Happstack.State (Version, deriveSerialize)
import System.Time (ClockTime(..))

deriving instance Typeable ClockTime
deriving instance Data ClockTime
instance Version ClockTime
$(deriveSerialize ''ClockTime)
$(deriveNewData [''ClockTime])
