module HAppS.Util.Tests.HostAddress
(propShowHostAddress, propShowHostAddress6)
where

import HAppS.Util.HostAddress
import System.IO.Unsafe
import Network.Socket (inet_ntoa, getNameInfo, NameInfoFlag(..), SockAddr(..), aNY_PORT)
import Data.Word (Word32)
import System.Random
import Test.QuickCheck

instance Arbitrary Word32 where
  arbitrary = choose (minBound, maxBound)

instance Random Word32 where
  randomR (a,b) g = (fromInteger i,g)
    where (i,_) = randomR (toInteger a, toInteger b) g
  random g =
    randomR (minBound,maxBound) g

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

