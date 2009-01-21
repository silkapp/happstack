module HAppS.Network.DNS.Name 
    (Name, nameSize, normalize, matchZone) where

import qualified Data.ByteString as BS
import Data.Char
import Data.Word
import HAppS.Network.DNS.Type

matchZone :: Question -> Zone -> Bool
matchZone  (Q c _ _) (Zone s _) = s `BS.isSuffixOf` c

nameSize :: Name -> Int
nameSize = BS.length

-- | normalize question to lowercase
normalize :: Question -> Question
normalize (Q qs qt qc) = Q (BS.map norm qs) qt qc
    where norm c = if c >= w 'A' && c <= w 'Z' then c + (w 'a' - w 'A') else c
	  w :: Char -> Word8
	  w = fromIntegral . ord

