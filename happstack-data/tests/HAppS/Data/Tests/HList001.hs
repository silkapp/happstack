{-# LANGUAGE TemplateHaskell, DeriveDataTypeable,
             FlexibleInstances, MultiParamTypeClasses,
             OverlappingInstances, UndecidableInstances #-}
module HAppS.Data.Tests.HList001 (hlist001, t10, t10_2, t11) where

import Language.Haskell.TH
import HAppS.Data
import Test.HUnit

$( deriveAll [''Show,''Eq, ''Default]
   [d|
       data UserInfo = UserInfo User Pass 
       newtype User = User String 
       newtype Pass = Pass String 
       newtype Age = Age Int
    |]
 )

t10, t10_2, t11 :: Test

t10   = "t10"    ~: hlextract (User "alex" .&. "abc") @?= "abc"
t10_2 = "t10_2"  ~: hlextract (User "alex" .&. Pass "pass" .&. "abc") @?= "abc"
t11   = "t11"    ~:
        let hl = User "alex" .&. Pass "pass"
        in Pass "pass2" @?= (hlextract $ hlupdate hl (Pass "pass2"))

hlist001 :: Test
hlist001 = "hlextract" ~: [t10, t10_2, t11]
