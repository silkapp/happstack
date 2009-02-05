module Happstack.Crypto.HMAC where

import Happstack.Crypto.SHA1
import Happstack.Crypto.Base64

import Data.Bits
import Data.Char

hmacSHA1 :: String -> String -> String
hmacSHA1 key str
    | length key > b = fail "hmacSHA1 doesn't support large keys yet"
    | otherwise
    = encode $ sha1Raw (doxor key opad ++ sha1Raw (doxor key ipad ++ str))
    where b = 64
          opad = replicate b '\x5C'
          ipad = replicate b '\x36'
          doxor a = zipWith fn (a++repeat '\0')
          fn x y = chr (ord x `xor` ord y)
