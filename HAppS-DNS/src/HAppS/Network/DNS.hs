-----------------------------------------------------------------------------
-- |
-- Module      :  HAppS.Network.DNS.ChooseMethod
-- Copyright   :  (c) HAppS.org, 2006
-- License     :  BSD3
-- Portability :  portable
--
-- DNS resolving works as follows:
--
-- * if environment variable @NAMESERVER@ is not defined use global name servers
--
-- * if environment variable @NAMESERVER@ is defined and has the value \"resolv\" use @\/etc\/resolv.conf@.
--
-- * if environment variable @NAMESERVER@ is defined and has a list of IP-addresses separated by space, use them as proxys
--
-----------------------------------------------------------------------------
module HAppS.Network.DNS
  (
   -- * Lookups
   module HAppS.Network.DNS.NSTree,
   -- * Mail (MX records)
   module HAppS.Network.DNS.MXClient,
   -- * Utilities
   module HAppS.Network.DNS.Type,
   initDNS
  ) where
  
import System.IO -- for interactive testing
import HAppS.Network.DNS.ChooseMethod(initDNS)
import HAppS.Network.DNS.MXClient
import HAppS.Network.DNS.NSTree
import HAppS.Network.DNS.Type(Packet(..), RR(..), RRVal(..), emptyPacket, enc, qd, question, rquestion, rname, rrval, rttl, rtype, encIP, decIP)

