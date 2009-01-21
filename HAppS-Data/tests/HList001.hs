
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

t10 = hlextract (User "alex" .&. "abc") == "abc"
t10_2 = hlextract (User "alex" .&. Pass "pass" .&. "abc") == "abc"
t11 = let hl = User "alex" .&. Pass "pass"
      in Pass "pass2" == (hlextract $ hlupdate hl (Pass "pass2"))

main :: IO ()
main = do print t10
          print t10_2
          print t11

