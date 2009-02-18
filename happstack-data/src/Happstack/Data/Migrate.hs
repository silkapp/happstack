{-# LANGUAGE MultiParamTypeClasses #-}
module Happstack.Data.Migrate where

{- | Migrate instances are needed to allow upgrades of MACID state.  It should be declared as instance Migrate Old New where migrate = transition_function
-}
class Migrate a b where
    migrate :: a -> b

