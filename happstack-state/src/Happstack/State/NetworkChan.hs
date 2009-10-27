module Happstack.State.NetworkChan
    ( BEncodeable(..)
    , NetworkChan
    , newNetworkChan
    , peek
    , poke
    ) where

import Data.BEncode
import Data.BEncode.Parser

import System.IO
import qualified Data.ByteString.Lazy as L
import Data.Binary

class BEncodeable a where
    bencode :: a -> BEncode
    bdecode :: BParser a


data NetworkChan = NetworkChan { inputHandle  :: Handle
                               , outputHandle :: Handle }

newNetworkChan :: Handle -> Handle -> IO NetworkChan
newNetworkChan inh outh
    = return NetworkChan { inputHandle  = inh
                         , outputHandle = outh }

peek :: BEncodeable a => NetworkChan -> IO a
peek chan
    = do size <- L.hGet (outputHandle chan) 8
         body <- L.hGet (outputHandle chan) (decode size)
         case bRead body of
           Nothing      -> error "NetworkChan.peek: Malformed bencoded data."
           Just encoded -> case runParser bdecode encoded of
                             Left err  -> error "NetworkChan.peek: Parsing bencoded data failed."
                             Right msg -> return msg

poke :: BEncodeable a => NetworkChan -> a -> IO ()
poke chan msg
    = do L.hPut (inputHandle chan) (encode (L.length body))
         L.hPut (inputHandle chan) body
         hFlush (inputHandle chan)
    where body = bPack (bencode msg)


