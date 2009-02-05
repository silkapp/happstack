{-# LANGUAGE MultiParamTypeClasses #-}
module HAppS.Data.Migrate where

class Migrate a b where
    migrate :: a -> b

