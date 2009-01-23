{-# OPTIONS -cpp #-}
-- | Compatiblity for ByteStrings
module HAppS.Util.ByteStringCompat
    (breakChar, breakCharEnd,
     dropSpace, dropSpaceEnd,
     rechunkLazy
    ) where

import Data.ByteString(ByteString)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy  as L
import Data.Char(isSpace)
import Foreign

#define STRICT2(f) f a b | a `seq` b `seq` False = undefined


{-# INLINE breakChar #-}
breakChar :: Char -> ByteString -> (ByteString, ByteString)
breakChar ch = B.break ((==) x) where x = B.c2w ch

-- | 'breakCharEnd' behaves like breakChar, but from the end of the
-- ByteString.
--
-- > breakCharEnd ('b') (pack "aabbcc") == ("aab","cc")
--
-- and the following are equivalent:
--
-- > breakCharEnd 'c' "abcdef"
-- > let (x,y) = break (=='c') (reverse "abcdef")
-- > in (reverse (drop 1 y), reverse x)
--
{-# INLINE breakCharEnd #-}
breakCharEnd :: Char -> ByteString -> (ByteString, ByteString)
breakCharEnd c p = B.breakEnd ((==) x) p where x = B.c2w c

{-# INLINE dropSpace #-}
dropSpace = C.dropWhile isSpace

{-# INLINE dropSpaceEnd #-}
dropSpaceEnd :: ByteString -> ByteString
dropSpaceEnd (B.PS x s l) = B.inlinePerformIO $ withForeignPtr x $ \p -> do
    i <- lastnonspace (p `plusPtr` s) (l-1)
    return $! if i == (-1) then B.empty else B.PS x s (i+1)

lastnonspace :: Ptr Word8 -> Int -> IO Int
STRICT2(lastnonspace)
lastnonspace ptr n
    | n < 0     = return n
    | otherwise = do w <- peekElemOff ptr n
                     if B.isSpaceWord8 w then lastnonspace ptr (n-1) else return n

-- | Chunk a lazy bytestring into reasonable chunks - is id from outside.
--   This is useful to make bytestring chunks reasonable sized for e.g.
--   compression.
rechunkLazy :: L.ByteString -> L.ByteString
rechunkLazy = L.fromChunks . norm . foldr w ([],[],0) . L.toChunks
    where norm (acc, [],  _) = acc
          norm (acc, cur, _) = B.concat cur : acc
          w chunk (acc,cur,len) = let bl = len + B.length chunk
                                  in if bl > 0x100 then (B.concat (chunk : cur) : acc, [], 0) else (acc, chunk : cur, bl)

