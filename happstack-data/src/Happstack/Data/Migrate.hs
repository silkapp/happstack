{-# LANGUAGE MultiParamTypeClasses #-}
module Happstack.Data.Migrate where

class Migrate a b where
    migrate :: a -> b

