module HAppS.Network.DNS.Type where

import qualified Data.ByteString as BS
import Data.Bits
import Data.Char
import Data.List(tails)
import Data.Word
import Foreign.Ptr

-- Questions

type Data = BS.ByteString

data Question = Q !Name !QType !QClass
              deriving(Ord)

instance Eq Question where
    (==) (Q an at ac) (Q bn bt bc) = an == bn && at `teq` bt && ac `teq` bc

teq :: Word16 -> Word16 -> Bool
teq 255 _ = True
teq _ 255 = True
teq x y   = x == y

satisfies :: Question -> RR -> Bool
satisfies (Q n t c) (RR rn rt rc _ _) = n == rn && (rt == 5 || t == 255 || t == rt) && c `teq` rc

type QType = Word16
type QClass= Word16
type Bufi  = (Ptr Word8, Int)

question :: String -> QType -> QClass -> Question
question a b c = Q (enc a) b c
rquestion :: Name -> QType -> QClass -> Question
rquestion a b c = Q a b c

{-
hashQuestion :: Question -> Int32
hashQuestion (Q n t _) = fromIntegral $ fromEnum t + foldl fun 0 lst
    where lst = take 10 $ drop 4 $ elems n 
	  fun a e = fromEnum e + (a `shiftR` 7)
-}

instance Show Question where
    show (Q qs qt qc) = "Question: '"++qd qs++"' type="++show qt++" class="++show qc

-- FIXME add safety checks for chunk < 64 and total < 256
enc :: String -> Name
enc = BS.pack . enc'

enc' :: String -> [Word8]
enc' s = concatMap (\p -> fromIntegral (length p) : map (fromIntegral . ord) p) $ reverse splitted
    where ein ("",a) '.' = ("",a)
          ein (c,a)  '.' = ("",reverse c : a)
          ein (c,a) ch   = (ch:c,a)
	  norm l@("":_)  = l
          norm lst       = "":lst
	  splitted       = let (t,r) = foldl ein ("",[]) s in norm $ reverse t : r

qd :: Name -> String
qd = qd' . BS.unpack

qd' :: [Word8] -> String
qd' [] = ""
qd' (n:ns) = let h = map (chr . fromEnum) $ take (fromEnum n) ns 
		 t = drop (fromEnum n) ns
		 in if length t <= 1 then h else h ++ '.':qd' t

-- | Return the subparts of a name from longest to shortest.
nameParts :: Name -> [String]
nameParts n = (filter (\xs -> null xs || ('.'== head xs)) $ tails u)
    where u = qd n

-- RRs

type RType = Word16
type RClass= Word16
data RR = RR  Name RType RClass Word32 Data
        | RR2 Name RType RClass Word32 RRVal

rtype :: RR -> RType
rtype (RR  _ rt _ _ _) = rt
rtype (RR2 _ rt _ _ _) = rt

rclass :: RR -> RClass
rclass (RR  _ _ rc _ _) = rc
rclass (RR2 _ _ rc _ _) = rc

rname :: RR -> Name
rname (RR  rn _ _ _ _) = rn
rname (RR2 rn _ _ _ _) = rn

rrval :: RR -> RRVal
rrval (RR2 _ _ _ _ x) = x
rrval x               = error ("rrval error - tried: "++show x)

rttl :: RR -> Word32
rttl (RR  _ _ _ t _) = t
rttl (RR2 _ _ _ t _) = t


instance Show RR where
    show (RR n t c ts d)  = unwords ["RR",qd n,show t,show c,show ts,show d]
    show (RR2 n t c ts d) = unwords ["RR",qd n,show t,show c,show ts,show d]

data RRVal = RRvA Word32
           | RRvNS Name
           | RRvCName Name
           | RRvMX Word16 Name
           | RRvSRV Word16 Word16 Word16 Name -- prio, weight, port, target (see RFC 2782)
           | RRvTXT Name

instance Show RRVal where
    show (RRvMX w n)   = unwords [show w, qd n]
    show (RRvNS n)     = qd n
    show (RRvA n)      = show n
    show (RRvCName n)  = qd n
    show (RRvSRV pr we po de) = unwords [show pr, show we, show po, qd de]
    show (RRvTXT n)    = qd n
--    show _             = "<unsupported RRVal in show>"

-- Zones

data Zone = Zone Name [RR] deriving(Show)
type Name = BS.ByteString


-- Packets

data Packet = Packet { idPQ :: !Word16,
		       hePQ :: !Word16,
		       qsPQ :: ![Question],
		       rsPQ :: ![RR],
		       nsPQ :: ![RR],
		       asPQ :: ![RR] 
		     } deriving(Show)

emptyPacket, errorPacket :: Packet
emptyPacket = Packet 0 0 [] [] [] []
errorPacket = emptyPacket { hePQ = 33155 }

{-
related :: Question -> RR -> RR -> Bool
related (Q _ qt qc) (RR _ 5 _ _ ca) (RR rn rt rc _ _) = elems ca == elems rn && qt `teq` rt && qc `teq` rc
related _ _ _ = False
-}

converge :: Word16 -> Question -> Packet -> Packet
converge qid q p = let rrs = rsPQ p ++ asPQ p
--		       sat = filter (satisfies q) rrs
--		       rel = filter (\rr -> any (\i -> related q i rr) sat) rrs
		   in Packet qid (hePQ p) [q] rrs [] []


-- | Show an IP in network byte order.
decIP :: Word32 -> String
decIP x = concat [sb 0, ".", sb 8, ".", sb 16, ".", sb 24]
    where sb i = show (shiftR x i .&. 0xFF)

-- | Encode an IP, result in network byte order.
encIP :: String -> Word32
encIP = foldr (\n o -> read n + (256 * o)) 0 . ws
    where ws = words . map (\c -> if c == '.' then ' ' else c)

