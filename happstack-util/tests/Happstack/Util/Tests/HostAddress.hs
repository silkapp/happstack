{-# LANGUAGE CPP #-}
module Happstack.Util.Tests.HostAddress
(propShowHostAddress, propShowHostAddress6)
where

import Happstack.Util.HostAddress
import System.IO.Unsafe
import Network.Socket (inet_ntoa, getNameInfo, NameInfoFlag(..), SockAddr(..), aNY_PORT)
import Data.Word (Word32)
import System.Random
import Test.QuickCheck

#if MIN_VERSION_QuickCheck(2,1,2)
#else
instance Arbitrary Word32 where
    arbitrary = choose (minBound, maxBound)

instance Random Word32 where
  randomR (a,b) g = (fromInteger i,g)
    where (i,_) = randomR (toInteger a, toInteger b) g
  random = randomR (minBound,maxBound)
#endif

propShowHostAddress :: HostAddress -> Bool
propShowHostAddress a = new == old
  where old = (unsafePerformIO . inet_ntoa) a
        new = showHostAddress a
  
propShowHostAddress6 :: HostAddress6 -> Bool
propShowHostAddress6 a = new == old
  where (Just old, _) =
          (unsafePerformIO . getNameInfo [NI_NUMERICHOST] True False) $
            SockAddrInet6 aNY_PORT 0 a 0
        new = showHostAddress6 a

