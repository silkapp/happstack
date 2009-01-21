module HAppS.Network.DNS.MXClient
    (withMXConnection) where

import HAppS.Network.DNS.NSTree
import HAppS.Network.DNS.Type

import Control.Exception as E
import Control.Monad
import Data.List
import Foreign
import Network.Socket 
import System.IO
import System.Log.Logger

-- | Lookup the given server from MX records.
-- Connect to the given port and pass the
-- resulting Handle to the handler.
-- The Handle is closed afterwards.
withMXConnection :: String -> Int -> (Handle -> IO a) -> IO a
withMXConnection host port handler = bracket (open host port) hClose handler

{-
-- | Open the upstream socket.
openUpstream = do
    host <- readFile "upstream-dns.conf"
    addr <- getHostByName $ filter (\c -> c/=' ' && c/='\r' && c/='\n') host
    openNameServer $ hostAddress addr
doQuery q = query q =<< openUpstream
-}

open :: String -> Int -> IO Handle
open host port = do
    res   <- ns $ question host 15 1
    tryAnyIO "MXClient: No MX servers to try" $ flip map (packetMXResponses res) $ \mx -> do
        logM "HAppS.Server.DNS.MXClient" INFO ("> mail server: "++ qd mx)
        mhost <- tryWithInfo mx $ additionalInfoFor res mx
        msock <- socket AF_INET Stream 0
        let sa = SockAddrInet (toEnum port) mhost
        logM "HAppS.Server.DNS.MXClient" INFO ("> Connecting to: "++show sa)
        connect msock sa
        hdl <- socketToHandle msock ReadWriteMode
        return hdl


tryWithInfo _  ((RR2 _ _ _ _ (RRvA w)) : _)      = return w
tryWithInfo _  ((RR2 _ _ _ _ (RRvCName n)) : _)  = queryARecord' n
tryWithInfo mx (_ : xs)                          = tryWithInfo mx xs
tryWithInfo mx []                                = queryARecord' mx

queryARecord' host = do
    res <- getA host
    when (null res) $ fail "MXClient: A query failed"
    return $ head res

tryAnyIO :: String -> [IO a] -> IO a
tryAnyIO err []     = fail err
tryAnyIO _   [x]    = x
tryAnyIO err (x:xs) = either (\_ -> tryAnyIO err xs) return =<< try x

packetMXResponses :: Packet -> [Name]
packetMXResponses p = map snd $ sort raw
    where isMx rr = rtype rr == 15
          mxs     = filter isMx $ rsPQ p
          raw     = [ (p,d) | RR2 _ _ _ _ (RRvMX p d) <- mxs ]

-- Here we accept both A and CNAME although CNAME is wrong(tm)
additionalInfoFor :: Packet -> Name -> [RR]
additionalInfoFor p n = filter want $ asPQ p
    where want rr = rname rr == n && (rtype rr == 1 || rtype rr == 5)
