module HAppS.Crypto.HMAC where

import HAppS.Crypto.SHA1
import HAppS.Crypto.Base64

-- import qualified Data.ByteString.Char8 as BS

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
          doxor a b = zipWith fn (a++repeat '\0') b
          fn a b = chr (ord a `xor` ord b)
