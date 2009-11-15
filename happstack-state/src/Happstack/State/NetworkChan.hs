module Happstack.State.NetworkChan
    ( BEncodeable(..)
    , NetworkChan
    , newNetworkChan
    , peek
    , poke
    ) where

import Data.BEncode
import Data.BEncode.Parser

import Control.Monad (when, forever, forM_ )
import System.IO
import qualified Data.ByteString.Lazy as L
import Data.Binary
import Control.Concurrent.STM
import Control.Concurrent (forkIO, ThreadId)

class BEncodeable a where
    bencode :: a -> BEncode
    bdecode :: BParser a


data NetworkChan = NetworkChan { outgoingChan :: TVar [L.ByteString]
                               , incomingHandle :: Handle
                               , writerThread :: ThreadId }

newNetworkChan :: Handle -> Handle -> IO NetworkChan
newNetworkChan outgoing incoming
    = do cache <- newTVarIO []
         tid <- forkIO $ forever $ do ls <- atomically $ do ls <- readTVar cache
                                                            when (null ls) retry
                                                            writeTVar cache []
                                                            return (reverse ls)
                                      forM_ ls $ \entry -> do L.hPut outgoing (encode (L.length entry))
                                                              L.hPut outgoing entry
                                      hFlush outgoing
         return NetworkChan { outgoingChan = cache
                            , incomingHandle = incoming
                            , writerThread = tid }

peek :: BEncodeable a => NetworkChan -> IO a
peek chan
    = do size <- L.hGet (incomingHandle chan) 8
         when (L.length size < 8) $ error "NetworkChan.peek: Connection closed."
         body <- L.hGet (incomingHandle chan) (decode size)
         case bRead body of
           Nothing      -> error "NetworkChan.peek: Malformed bencoded data."
           Just encoded -> case runParser bdecode encoded of
                             Left err  -> error "NetworkChan.peek: Parsing bencoded data failed."
                             Right msg -> return msg

poke :: BEncodeable a => NetworkChan -> a -> IO ()
poke chan msg
    = atomically $
      do ls <- readTVar (outgoingChan chan)
         writeTVar (outgoingChan chan) (bPack (bencode msg):ls)
{-
do L.hPut (inputHandle chan) (encode (L.length body))
          L.hPut (inputHandle chan) body
          hFlush (inputHandle chan)
    where body = bPack (bencode msg)
-}

