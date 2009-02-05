-- Pure Haskell functions to convert HostAddress and HostAddress6 to a human
-- readable string format.
module HAppS.Util.HostAddress (showHostAddress, showHostAddress6, HostAddress, HostAddress6) where
import Data.Word (Word32)
import Numeric (showHex)
import Data.List (intersperse)

type HostAddress = Word32
type HostAddress6 = (Word32, Word32, Word32, Word32)

showHostAddress :: HostAddress -> String
showHostAddress num = concat [show q1, ".", show q2, ".", show q3, ".", show q4]
  where (num',q1)   = (num `quotRem` 256)
        (num'',q2)  = (num' `quotRem` 256)
        (num''',q3) = (num'' `quotRem` 256)
        (_,q4)      = (num''' `quotRem` 256)
 
showHostAddress6 :: HostAddress6 -> String
showHostAddress6 (a,b,c,d) =
  (concat . intersperse ":" . map (flip showHex $ "")) $
    [p1,p2,p3,p4,p5,p6,p7,p8]
  where (a',p2) = (a `quotRem` 65536)
        (_,p1)  = (a' `quotRem` 65536)
        (b',p4) = (b `quotRem` 65536)
        (_,p3)  = (b' `quotRem` 65536)
        (c',p6) = (c `quotRem` 65536)
        (_,p5)  = (c' `quotRem` 65536)
        (d',p8) = (d `quotRem` 65536)
        (_,p7)  = (d' `quotRem` 65536)

