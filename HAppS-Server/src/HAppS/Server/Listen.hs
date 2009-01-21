module HAppS.Server.Listen where
-- Copyright (C) 2005 HAppS.org. All Rights Reserved.

import Network
import Control.Exception as E
import HAppS.Util.Concurrent
import System.IO
--data ConnInfo = ConnInfo {clientSocket,serverSocket::SocketInfo} deriving (Eq,Read,Show)
data SocketInfo = 
    SocketInfo { hostId    :: HostId
	       ,portNumber :: PortId
	       ,mbCert     :: Maybe X509Cert
               } deriving (Eq,Read,Show)


data Host = Host {hostName::HostId,hostPort::PortId}

type HostId=String
type PortId=Integer
data X509Cert = X509Cert  deriving (Eq,Read,Show) -- ssl cert

data SocketEvent event =
    SocketEvent { serverInfo :: SocketInfo
                 ,clientInfo :: SocketInfo
                 ,socketEvent:: event
                } deriving (Read,Show)

handle=socketEvent
a_socketEvent ev rec = rec {socketEvent=ev}

socketSetup (Host hostId portNum) =
	do
	socket <- listenOn (PortNumber (fromIntegral portNum))
        registerResetAction $ sClose socket
	return (accept' socket)
	where
	serverInfo=SocketInfo hostId portNum Nothing
	accept' socket=
		  do
		  (handle,hostName,portNum)<-accept socket
		  return $ SocketEvent serverInfo 
				 (SocketInfo hostName (fromIntegral portNum) Nothing)
				 handle
		  --(serverInfo,
			 --	  SocketInfo hostName (fromIntegral portNum) Nothing,handle)
--add ssl socket setup later...

socketServe protocol host  = 
	withSocketsDo $
	do 
	accept <- socketSetup host 
        let work h = fork_ (protocol h `E.catch` (\_ -> hClose $ socketEvent h))
	forkEver (accept >>= work)


{--
  protocol sets timer
--}

