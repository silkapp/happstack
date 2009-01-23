-- BSDLicensed
-- Copyright (c) 2002, Warrick Gray All rights reserved.
-- See http://www.ietf.org/rfc/rfc2045.txt for definition.
module HAppS.Crypto.Base64 (
    encode,
    decode,
    chop72
) where

{-

The following properties should hold:

  decode . encode = id
  decode . chop72 . encode = id

I.E. Both "encode" and "chop72 . encode" are valid methods of encoding input,
the second variation corresponds better with the RFC above, but outside of
MIME applications might be undesireable.



But: The Haskell98 Char type is at least 16bits (and often 32), these implementations assume only 
     8 significant bits, which is more than enough for US-ASCII.  
-}




import Data.Array.Unboxed
import Data.Bits
import Data.Int
import Data.Char (chr,ord)


encodeArray :: UArray Int Char
encodeArray = array (0,64) 
          [ (0,'A'),  (1,'B'),  (2,'C'),  (3,'D'),  (4,'E'),  (5,'F')                    
          , (6,'G'),  (7,'H'),  (8,'I'),  (9,'J'),  (10,'K'), (11,'L')                    
          , (12,'M'), (13,'N'), (14,'O'), (15,'P'), (16,'Q'), (17,'R')
          , (18,'S'), (19,'T'), (20,'U'), (21,'V'), (22,'W'), (23,'X')
          , (24,'Y'), (25,'Z'), (26,'a'), (27,'b'), (28,'c'), (29,'d')
          , (30,'e'), (31,'f'), (32,'g'), (33,'h'), (34,'i'), (35,'j')
          , (36,'k'), (37,'l'), (38,'m'), (39,'n'), (40,'o'), (41,'p')
          , (42,'q'), (43,'r'), (44,'s'), (45,'t'), (46,'u'), (47,'v')
          , (48,'w'), (49,'x'), (50,'y'), (51,'z'), (52,'0'), (53,'1')
          , (54,'2'), (55,'3'), (56,'4'), (57,'5'), (58,'6'), (59,'7')
          , (60,'8'), (61,'9'), (62,'+'), (63,'/') ]


-- Convert between 4 base64 (6bits ea) integers and 1 ordinary integer (32 bits)
-- clearly the upmost/leftmost 8 bits of the answer are 0.
-- Hack Alert: In the last entry of the answer, the upper 8 bits encode 
-- the integer number of 6bit groups encoded in that integer, ie 1, 2, 3.
-- 0 represents a 4 :(
int4_char3 :: [Int] -> [Char]
int4_char3 (a:b:c:d:t) = 
    let n = (a `shiftL` 18 .|. b `shiftL` 12 .|. c `shiftL` 6 .|. d)
    in (chr (n `shiftR` 16 .&. 0xff))
     : (chr (n `shiftR` 8 .&. 0xff))
     : (chr (n .&. 0xff)) : int4_char3 t

int4_char3 [a,b,c] =
    let n = (a `shiftL` 18 .|. b `shiftL` 12 .|. c `shiftL` 6)
    in [ (chr (n `shiftR` 16 .&. 0xff))
       , (chr (n `shiftR` 8 .&. 0xff)) ]

int4_char3 [a,b] = 
    let n = (a `shiftL` 18 .|. b `shiftL` 12)
    in [ (chr (n `shiftR` 16 .&. 0xff)) ]

int4_char3 [] = []     




-- Convert triplets of characters to
-- 4 base64 integers.  The last entries
-- in the list may not produce 4 integers,
-- a trailing 2 character group gives 3 integers,
-- while a trailing single character gives 2 integers.
char3_int4 :: [Char] -> [Int]
char3_int4 (a:b:c:t) 
    = let n = (ord a `shiftL` 16 .|. ord b `shiftL` 8 .|. ord c)
      in (n `shiftR` 18 .&. 0x3f) : (n `shiftR` 12 .&. 0x3f) : (n `shiftR` 6  .&. 0x3f) : (n .&. 0x3f) : char3_int4 t

char3_int4 [a,b]
    = let n = (ord a `shiftL` 16 .|. ord b `shiftL` 8)
      in [ (n `shiftR` 18 .&. 0x3f)
         , (n `shiftR` 12 .&. 0x3f)
         , (n `shiftR` 6  .&. 0x3f) ]
    
char3_int4 [a]
    = let n = (ord a `shiftL` 16)
      in [(n `shiftR` 18 .&. 0x3f),(n `shiftR` 12 .&. 0x3f)]

char3_int4 [] = []


-- Retrieve base64 char, given an array index integer in the range [0..63]
enc1 :: Int -> Char
enc1 ch = encodeArray!ch


-- | Cut up a string into 72 char lines, each line terminated by CRLF.
chop72 :: String -> String
chop72 str =  let (bgn,end) = splitAt 70 str
              in if null end then bgn else bgn ++ "\r\n" ++ chop72 end


-- Pads a base64 code to a multiple of 4 characters, using the special
-- '=' character.
quadruplets (a:b:c:d:t) = a:b:c:d:quadruplets t
quadruplets [a,b,c]     = [a,b,c,'=']      -- 16bit tail unit
quadruplets [a,b]       = [a,b,'=','=']    -- 8bit tail unit
quadruplets []          = []               -- 24bit tail unit


enc :: [Int] -> [Char]
enc = quadruplets . map enc1


dcd [] = []
dcd (h:t)
    | h <= 'Z' && h >= 'A'  =  ord h - ord 'A'      : dcd t
    | h >= '0' && h <= '9'  =  ord h - ord '0' + 52 : dcd t
    | h >= 'a' && h <= 'z'  =  ord h - ord 'a' + 26 : dcd t
    | h == '+'  = 62 : dcd t
    | h == '/'  = 63 : dcd t
    | h == '='  = []  -- terminate data stream
    | otherwise = dcd t


-- Principal encoding and decoding functions.
encode, decode :: String -> String
encode = enc . char3_int4
decode = int4_char3 . dcd
