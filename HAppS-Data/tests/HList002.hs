
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable,
             FlexibleInstances, MultiParamTypeClasses,
             OverlappingInstances, UndecidableInstances #-}

module Main (main) where

import Language.Haskell.TH
import HAppS.Data

$( deriveAll [''Show,''Eq, ''Default]
   [d|
       data UserInfo = UserInfo User Pass 
       newtype User = User String 
       newtype Pass = Pass String 
       newtype Age = Age Int
    |]
 )

t12 = (User "ales" .&. Pass "pass" .&. Age 55 )
t13 = toPairs t12
t14 = fromPairs t13 
t15 = t14 == t12

main :: IO ()
main = do print t12
          print t13
          print t14
          print t15

