{-# LANGUAGE TemplateHaskell, DeriveDataTypeable,
             FlexibleInstances, MultiParamTypeClasses,
             OverlappingInstances, UndecidableInstances #-}
module HAppS.Data.Tests.HasT001 (hasT001, t8, t9) where

import Language.Haskell.TH
import HAppS.Data
import Test.HUnit (Test,(@?=), (~:))

$( deriveAll [''Show,''Eq, ''Default]
             [d| newtype User = User String |]
 )

tHasT :: HasT hlist y => hlist -> y -> (y, y)
tHasT hlist v = (hlextract hlist, v)

t8 :: Test
t8 = "t8" ~: tHasT (User "alex" .&. "asad") (User "alex2") @?= (User "alex",User "alex2")

t9 :: Test
t9 = "t9" ~: tHasT (User "alex" .&. "asad" ) "abc" @?= ("asad","abc")

hasT001 :: Test
hasT001 = "hasT001" ~: [t8, t9]
