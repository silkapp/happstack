{-# OPTIONS -cpp #-}

#undef P
#define LST  \
P(A,     1)  \
P(NS,    2)  \
P(CNAME, 5)  \
P(SOA,   6)  \
P(NULL, 10)  \
P(PTR,  12)  \
P(MX,   15)  \
P(TXT,  16)  \
P(AAAA, 28)

module HAppS.Network.DNS.RR (
#undef P
#define P(x,y) rr/**/x, 
LST RRType, getRR, putRR,
parseRType, parseRClass
) where

import qualified Data.ByteString as BS
import Data.Word
import HAppS.Network.DNS.LoWire
import HAppS.Network.DNS.MutableEnv
import HAppS.Network.DNS.Type

newtype RRType = RRT Word16


-- Creating instances
#undef P
#define P(x,y) rr/**/x :: RRType;
LST
#undef P
#define P(x,y) rr/**/x = RRT y;
LST

-- Show
#undef P
#define P(x,y) show (RRT y) = "x";
instance Show RRType where
    LST show (RRT x) = show x

parseRClass :: String -> Word16
parseRClass "IN" = 1
parseRClass x    = read x

#undef P
#define P(x,y) parseRType "x" = y; 
parseRType :: String -> Word16
LST parseRType x = read x

-- READING

getRR :: MutEnv RR
getRR = do name    <- getName
	   rtype   <- getW16
	   rclass  <- getW16
	   tstamp  <- getW32
	   rdlen   <- getW16
           case rtype of
             1  -> do host    <- getW32
                      return $ RR2 name rtype rclass tstamp $ RRvA $ htonl host
             2  -> do host    <- getName
                      return $ RR2 name rtype rclass tstamp $ RRvNS host
             5  -> do host    <- getName
                      return $ RR2 name rtype rclass tstamp $ RRvCName host
             15 -> do prio    <- getW16
                      host    <- getName
                      return $ RR2 name rtype rclass tstamp $ RRvMX prio host
             16 -> do payload <- getRRData rtype rdlen
                      return $ RR2 name rtype rclass tstamp $ RRvTXT payload
             33 -> do prio    <- getW16
                      weight  <- getW16
                      port    <- getW16  
                      host    <- getName
                      return $ RR2 name rtype rclass tstamp $ RRvSRV prio weight port host
             _  -> do rdata   <- getRRData rtype rdlen
                      return $ RR name rtype rclass tstamp rdata


getRRData :: Word16 -> Word16 -> MutEnv Data
getRRData 12 _ = getName  -- ptr
getRRData  6 _ = do n1 <- getName
		    n2 <- getName
		    re <- getW8Lst 16
		    return $ BS.concat [n1, n2, BS.pack re]
getRRData  _ l = fmap BS.pack $ getW8Lst (fromEnum l)

-- WRITING

putRR :: RR -> MutEnv ()
putRR (RR n t c ts rd) = putName n >> putW16 t >> putW16 c >> 
			 putW32 ts >> putW16 (toEnum $ size rd) >> putName rd --FIXME

size :: Name -> Int
size = BS.length


{-
The following RR definitions are expected to occur, at least
potentially, in all classes.  In particular, NS, SOA, CNAME, and PTR
will be used in all classes, and have the same format in all classes.
Because their RDATA format is known, all domain names in the RDATA
section of these RRs may be compressed.

cname = Name
ns    = Name
ptr   = Name
soa   = Name Name W32 W32 W32 W32

Additional : MX, NS

-}
