
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable,
             FlexibleInstances, MultiParamTypeClasses,
             OverlappingInstances, UndecidableInstances #-}

module Main (main) where

import Language.Haskell.TH
import HAppS.Data

$( deriveAll [''Show,''Eq, ''Default]
             [d| newtype User = User String |]
 )

tHasT :: HasT hlist y => hlist -> y -> (y, y)
tHasT hlist v = (hlextract hlist, v)

t8 = tHasT (User "alex" .&. "asad") (User "alex2")

t9 = tHasT (User "alex" .&. "asad" ) "abc"

main :: IO ()
main = do print t8
          print t9

