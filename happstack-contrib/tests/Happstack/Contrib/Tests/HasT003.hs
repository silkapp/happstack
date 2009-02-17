
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable,
             FlexibleInstances, MultiParamTypeClasses,
             OverlappingInstances, UndecidableInstances #-}

module Main (main) where

import Language.Haskell.TH
import Happstack.Data

$( deriveAll [''Show,''Eq, ''Default]
             [d| newtype User = User String |]
 )

tHasT :: HasT hlist y => hlist -> y -> (y, y)
tHasT hlist v = (hlextract hlist, v)

e9 = tHasT (User "alex" .&. 456) 123

main :: IO ()
main = print e9

