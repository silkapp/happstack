{-# LANGUAGE TemplateHaskell, DeriveDataTypeable,
             FlexibleInstances, MultiParamTypeClasses,
             OverlappingInstances, UndecidableInstances #-}
module Happstack.Contrib.Tests.HList002 (hlist002, t15) where

import Language.Haskell.TH
import Happstack.Data
import Happstack.Contrib.HList
import Test.HUnit

$( deriveAll [''Show,''Eq, ''Default]
   [d|
       data UserInfo = UserInfo User Pass 
       newtype User = User String 
       newtype Pass = Pass String 
       newtype Age = Age Int
    |]
 )
t12 :: Couple User (Couple Pass Age)
t12 = (User "ales" .&. Pass "pass" .&. Age 55 )

t13 :: Pairs
t13 = toPairs t12

t14 :: Maybe (Couple User (Couple Pass Age))
t14 = fromPairs t13 

t15 :: Test
t15 = "t15" ~: (Just t12) @?= t14

hlist002 :: Test
hlist002 = "toPairs/fromPairs" ~: [ t15 ]
