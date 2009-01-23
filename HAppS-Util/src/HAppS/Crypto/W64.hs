{-# LANGUAGE CPP, UndecidableInstances #-}
module HAppS.Crypto.W64 where

import HAppS.Crypto.DES
import HAppS.Crypto.SHA1
import Data.List
import Numeric(readHex)
#ifdef TEST
import Test.QuickCheck
#endif

--the first character to be encrypted is 0 1 2 3 the number that should
--be ignored at the end of the string



pad x = padding++x
    where 
    padLength = 4 - (length x) `mod` 4
    padding = (toEnum padLength) : (take (padLength-1) $ repeat 'A')

unpad x = drop (fromEnum $ head x) x

prop_PadUnPad x = x==(unpad $ pad x)

is4Char x = length x==4
quadCharToW64 x = fromInteger $ impl $ map (fromIntegral.fromEnum) x
    where impl [a,b,c,d]=(a*2^24+b*2^16+c*2^8+d)

w64ToQuadChar w64 = 
    map (toEnum.fromIntegral) $! reverse $! take 4 $! v ++ (repeat 0)
    where v = w64ToQuadNum w64

w64ToQuadNum w64 = unfoldr (\x->if x==0 then Nothing else 
                            Just (x `mod` 256,x `div` 256)) 
                   w64


#ifdef TEST
prop_quadCharW64 x = is4Char x ==> x == (w64ToQuadChar $ quadCharToW64 x)
#endif

--assume padded
toQuadChars [] = []
toQuadChars (a:b:c:d:rest) = [a,b,c,d]:toQuadChars rest

stringToW64s x = map quadCharToW64 $ toQuadChars $ pad x
w64sToString x = unpad $ concat $ map w64ToQuadChar x

prop_stringW64 x = x == (w64sToString $ stringToW64s x)

--des takes a 64 bit number and encrypts it with another 64 bit number

--so string DES is an key string converted to a w64 and a value converted
--to a list of w64s
--the result is then converted from a list of w64s back to a string
--the key is an sha1 hash of the string converted to a 64 bit int or 
-- the first 16 hex digits  -- `1/3 of total key space

hexToW64 x = fromInteger $  fst $ head $ readHex $ take 16 x
stringToKey x = hexToW64 $ sha1 x

des_encrypt key v = map (flip des_enc $ stringToKey key) $ stringToW64s v

des_decrypt key v = 
    w64sToString $ 
    map (toInteger . flip des_dec (stringToKey key)) 
              v

prop_DES key val = val == (des_decrypt key $ des_encrypt key val)

