module Types where

newtype Greeting = Greeting { toString :: String }
    deriving (Eq, Ord, Read, Show)
