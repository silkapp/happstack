{-# LANGUAGE CPP, UndecidableInstances #-}
module Happstack.Crypto.W64 where

import Happstack.Crypto.DES
import Happstack.Crypto.SHA1
import Data.List
import Data.Bits
import Numeric(readHex)
#ifdef TEST
import Test.QuickCheck
#endif

--the first character to be encrypted is 0 1 2 3 the number that should
--be ignored at the end of the string


pad :: String -> String
pad x = padding++x
    where 
    padLength = 4 - (length x) `mod` 4
    padding = (toEnum padLength) : (replicate (padLength-1) 'A')

unpad :: (Enum a) => [a] -> [a]
unpad x = drop (fromEnum $ head x) x

prop_PadUnPad :: String -> Bool
prop_PadUnPad x = x==(unpad $ pad x)

is4Char :: [a] -> Bool
is4Char x = length x==4

quadCharToW64 :: (Num b, Enum a) => [a] -> b
quadCharToW64 = fromInteger . impl . map (fromIntegral.fromEnum)
    where impl :: [Integer] -> Integer
          impl [a,b,c,d] = (a `shiftL` 24) + (b `shiftL` 16) + (c `shiftL` 8) + d
          impl _ = error "Argument to quadCharToW64 must be length 4"

w64ToQuadChar :: (Integral a, Enum b) => a -> [b]
w64ToQuadChar w64 = 
    map (toEnum.fromIntegral) $! reverse $! take 4 $! v ++ (repeat 0)
    where v = w64ToQuadNum w64

w64ToQuadNum :: (Integral a) => a -> [a]
w64ToQuadNum = unfoldr (\x->if x==0 then Nothing else 
                            Just (x `mod` 256,x `div` 256))

#ifdef TEST
prop_quadCharW64 x = is4Char x ==> x == (w64ToQuadChar $ quadCharToW64 x)
#endif

--assume padded
toQuadChars :: [a] -> [[a]]
toQuadChars [] = []
toQuadChars (a:b:c:d:rest) = [a,b,c,d]:toQuadChars rest
toQuadChars _ = error "Argument for toQuadChars must have a length that is a multiple of 4"

stringToW64s :: (Num a) => String -> [a]
stringToW64s = map quadCharToW64 . toQuadChars . pad

w64sToString :: (Enum b) => [Integer] -> [b]
w64sToString = unpad . concatMap w64ToQuadChar

prop_stringW64 :: String -> Bool
prop_stringW64 x = x == (w64sToString $ stringToW64s x)

--des takes a 64 bit number and encrypts it with another 64 bit number

--so string DES is an key string converted to a w64 and a value converted
--to a list of w64s
--the result is then converted from a list of w64s back to a string
--the key is an sha1 hash of the string converted to a 64 bit int or 
-- the first 16 hex digits  -- `1/3 of total key space

hexToW64 :: (Num a) => String -> a
hexToW64 = fromInteger . fst . head . readHex . take 16

stringToKey :: (Num a) => String -> a
stringToKey = hexToW64 . sha1

des_encrypt :: String -> String -> [Enc]
des_encrypt key = map (flip des_enc $ stringToKey key) . stringToW64s

des_decrypt :: (Enum a) => String -> [Message] -> [a]
des_decrypt key = 
    w64sToString . 
    map (toInteger . flip des_dec (stringToKey key)) 
              

prop_DES :: String -> String -> Bool
prop_DES key val = val == (des_decrypt key $ des_encrypt key val)
