module HAppS.Network.DNS.Util where

import HAppS.Network.DNS.ChooseMethod(dnsLog)
import HAppS.Network.DNS.HiWire
import HAppS.Network.DNS.Type

import Control.Monad
import Foreign
import Network.Socket
import System.Time

query q (sock, sa) = allocaArray 512 $ \buf -> do
    (len,qid) <- createQuery [q] (buf,512)
    sendBufTo sock buf len sa
    (rl,rsa) <- recvBufFrom sock buf 512
    when (rsa /= sa) $ fail "DNS.Util: reply from wrong SockAddr"
    res <- parsePacket (buf,rl)
    when (idPQ res /= qid) $ fail "DNS.Util: reply has wrong question id"
    return res


openNameServer :: HostAddress -> IO (Socket, SockAddr)
openNameServer addr = do
    sock <- socket AF_INET Datagram 0
    dnsLog ("> Connecting to DNS server: "++show (SockAddrInet 53 addr))
    return (sock, SockAddrInet 53 addr)

type EpochSeconds = Integer

getEpochSeconds :: IO EpochSeconds
getEpochSeconds = do TOD x _ <- getClockTime; return x
