{-# LANGUAGE CPP #-}
module HAppS.Network.DNS.HiWire 
    (parseQuery, createQuery, parsePacket, putPacket) where

import Control.Monad
import Control.Monad.Trans
import Foreign
import HAppS.Network.DNS.LoWire
import HAppS.Network.DNS.MutableEnv
import HAppS.Network.DNS.Name
import HAppS.Network.DNS.RR
import HAppS.Network.DNS.Type
import System.Random (randomRIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BB

type QId   = Word16

---------------------------------

parsePacket :: Bufi -> IO Packet
parsePacket (p,l) = runMutableEnv p l dpp 
    where dpp = do [qid,qhead,qs,as,ns,ars] <- getW16Lst 6
		   lift $ prec (validPHead qhead) "error response"
		   rqs <- getQuestions $ fromIntegral qs
		   ras <- getRRs       $ fromIntegral as
		   rns <- getRRs       $ fromIntegral ns
		   rars<- getRRs       $ fromIntegral ars
		   return $ Packet qid qhead rqs ras rns rars

parseQuery :: Bufi -> IO (QId,[Question])
parseQuery (ptr,len) = runMutableEnv ptr len dpp
    where dpp = do [qid,qhead,qs,as,ns,ars] <- getW16Lst 6
		   let flag = validQHead qhead && qs == 1 && as == 0 && ns == 0 && ars == 0
		   lift $ prec (not flag) "error query"
		   q <- getQuestion
		   return (qid,[q])

prec :: Bool -> String -> IO ()
prec b s = when b (fail s)

-- Parsing Questions

getQuestions :: Int -> MutEnv [Question]
getQuestions n = replicateM n getQuestion 

getQuestion :: MutEnv Question
getQuestion = (liftM3 rquestion) getName getW16 getW16

-- PARSING RRs

getRRs :: Int -> MutEnv [RR]
getRRs iv = replicateM iv getRR

---------------------------------

createQuery :: [Question] -> Bufi -> IO (Int,QId)
createQuery qs (ptr,len) = do let qlen = headWireLen + sum (map qWireLen qs) 
			      when (qlen > len) $ fail "createQuery: too small buffer"
			      qid <- runMutableEnv ptr len (process qs) 
			      return (qlen,qid)

process :: [Question] -> MutEnv Word16
process qs = do qid <- liftIO $ randomRIO (0,65536) >>= return . i2w
                putW16Lst [qid,256,i2w $ length qs, 0, 0, 0]
                mapM_ pokeQuestion qs
                return qid

pokeQuestion :: Question -> MutEnv ()
pokeQuestion (Q qs qt qc) = putName qs >> putW16 qt >> putW16 qc

putPacket :: Bufi -> Packet -> IO Int
putPacket (ptr,len) p = runMutableEnv ptr len (pokePacket len p)

pokePacket :: Int -> Packet -> MutEnv Int
pokePacket len (Packet pid he qs as ns ars) = pokePacket' len $ Packet pid he qs (map rr2ToPlain as) (map rr2ToPlain ns) (map rr2ToPlain ars)
pokePacket' len (Packet pid he qs as ns ars) = 
    do let rrlen = rsl as + rsl ns + rsl ns + rsl ars
	   plen = crit + rrlen
	   crit = headWireLen + sum (map qWireLen qs)
	   qhead = if (plen < len) 
		    then [pid,he,i2w $ length qs, i2w $ length as, i2w $ length ns, i2w $ length ars]
		    else [pid,he + 512, i2w $ length qs, 0, 0, 0] -- trunc
       when (crit > len) $ fail "putPacket too small buffer"
       putW16Lst qhead
       mapM_ pokeQuestion qs
       if plen < len
	then mapM_ putRR as >> mapM_ putRR ns >> mapM_ putRR ars >> return plen
	else return crit

rr2ToPlain x@(RR {}) = x
rr2ToPlain (RR2 n t c ts x) = RR n t c ts $ conv x
    where conv (RRvA x)     = w32ToBS x
          conv (RRvNS n)    = n
          conv (RRvCName n) = n
          conv (RRvMX w n)  = w16ToBS w `BS.append` n
          conv (RRvTXT d)   = d
          conv (RRvSRV a b c n) = BS.concat [w16ToBS a, w16ToBS b, w16ToBS c, n]
          w16ToBS x = BB.unsafeCreate 2 $ \ptr -> castPtr ptr `poke` htons x
          w32ToBS x = BB.unsafeCreate 4 $ \ptr -> castPtr ptr `poke` htonl x


rsl :: [RR] -> Int
rsl = sLen rrLen
    where rrLen (RR n _ _ _ d)  = 10 + nameSize n + nameSize d
          sLen f = foldl (\a c -> a + f c) 0 


-- Length of various components
i2w :: Int -> Word16
i2w = fromIntegral
headWireLen :: Int
headWireLen = 12
qWireLen :: Question -> Int
qWireLen (Q qs _ _) = nameSize qs + 4

-- check whether Query head is valid for us, checks that:
-- query = op = auth = trunc = error = 0
-- Z, RD and RA may be anything
validQHead, validPHead :: Word16 -> Bool
validQHead w = w `mod` 16 == 0 && w `shiftR` 9 == 0
validPHead w = (w .&. 3840) == 0

{- Packet format

PACKETS = (Header, Questions, Answer, Authority, Additional)

HEADER  = (Id, Head, Qs, Ans, Nss, Ars)

HEAD in Queries: 

  0       0000     0    0     1   0  000    0000   = 256
  query   op=query auth trunc RD  RA z(res) error

  1       0000     0    0     1   1  000    0000   = 0x8180 = 33152

0x8183 = no such name = 33155
-}


#ifdef DEBUG

--------------------------------------------------------------------------------
-------------------  Pretty print a packet  ------------------------------------
--------------------------------------------------------------------------------


foreign import ccall unsafe "ntohs" ntohs :: Word16 -> Word16

prettyPacket :: (Ptr Word8,Int) -> IO ()
prettyPacket (ptr,len) = if len < headWireLen 
			  then putStrLn "packet too short to contain a header"
			  else proc
    where proc = do lst <- peekArray 6 (castPtr ptr)
		    [id,head,qs,as,ns,ars] <- return (map ntohs lst)
		    putStrLn ("Packet Id=" ++ show id)
		    putStrLn $ prettyQHead (fromEnum head)
		    putStrLn ("Questions   = " ++ show qs ++
			      " Answers     = " ++ show as++
			      " Authorative = " ++ show ns++
			      " Additional  = " ++ show ars)
		    putStrLn ""

prettyQHead :: Int -> String
prettyQHead w = unlines ["Header: ",
			 is w 0 " query" " response",
			 let n = sm w 1 15 in " "++(op!!n)++" ("++show n++")" ,
			 is w 5 " non-authorative" " authorative",
			 is w 6 " not-truncated" " TRUNCATED",
			 is w 7 " recursion desired" " no recursion desired",
			 is w 8 " recursion available" " no recursion available",
			 " Z = " ++ show (sm w 9 7),
			 let n = sm w 12 15 in " "++(rc!!n)++" ("++show n++")" 
	]		 
    where op = let u = "reserved type":u in "standard query":"inverse query":"server status":u
	  rc = let u = "unknown":u in "success":"format error":"server failure":"name error":"not implemented":"refused":u
	  sm w n m = (w `shiftR` n) .&. m
	  is w n s f = if testBit w n then f else s


#endif /* DEBUG */
