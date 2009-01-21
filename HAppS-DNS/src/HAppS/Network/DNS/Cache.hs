module HAppS.Network.DNS.Cache
    (packetTTL, lookupGC, insertGC, withGC
    ) where

import Control.Concurrent.MVar
import qualified Data.Map as M
import System.IO.Unsafe

import HAppS.Network.DNS.Type
import HAppS.Network.DNS.Util
import HAppS.Util.Concurrent

type Cache a = MVar (M.Map Question (a,EpochSeconds))

packetTTL :: Packet -> EpochSeconds
packetTTL packet = fromIntegral $ minimum $ map minimum [r rsPQ, r nsPQ, r asPQ]
    where day = 24*60*60
          r f = day : (map rttl $ f packet)

{-# NOINLINE globalCache #-}
globalCache :: Cache Packet
globalCache = unsafePerformIO $ do c <- newMVar M.empty
                                   fork $ killer c
                                   return c

killer mv = do sleep 60
               ctime <- getEpochSeconds
               let f v = snd v > ctime
               modifyMVar_ globalCache (return . M.filter f)
               killer mv

withGC :: IO Packet -> Question -> IO Packet
withGC lf q = maybe work return =<< lookupGC q
    where work = do x <- lf; insertGC q x; return x

-- | Look for an answer in the global cache.
lookupGC :: Question -> IO (Maybe Packet)
lookupGC q = fmap (fmap fst . M.lookup q) $ readMVar globalCache

-- | Insert an answer in the global cache.
insertGC :: Question -> Packet -> IO ()
insertGC q p = do let ttl = packetTTL p
                  if ttl <= 0 
                    then return ()
                    else do ctime <- getEpochSeconds
                            modifyMVar_ globalCache (return . M.insert q (p,ctime+ttl))
