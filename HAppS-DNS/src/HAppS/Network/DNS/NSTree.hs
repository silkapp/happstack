module HAppS.Network.DNS.NSTree(ns, nsNC, getA, nsA, prettyNSTree) where

import Control.Concurrent(killThread)
import Control.Concurrent.MVar
import Control.Monad
import Data.List
import Data.Word
import HAppS.Network.DNS.Cache
import HAppS.Network.DNS.ChooseMethod
import HAppS.Network.DNS.Type
import HAppS.Network.DNS.Util
import HAppS.Util.Concurrent
import System.IO.Unsafe
import System.Log.Logger
import System.Time
import qualified Data.Map as M


-- | Network ping time in seconds
data ServerPing = SP Double Double
    deriving(Eq,Show)

newServerPing = SP 1.0e-100 0
ageServerPing x = x
-- This just basic running average calculation
addSample rtt sp@(SP srtt d) | rtt < 0 = ageServerPing sp
                             | True  = 
  let g    = 0.125
      h    = 0.25
      err  = rtt - srtt
      srtt'= srtt + g * err
      d'   = d + h * (abs err - d)
      in SP srtt' d'

-- rto  (SP srtt d) = srtt + 4*d
srtt (SP srtt _) = srtt

instance Ord ServerPing where
    compare a b = srtt a `compare` srtt b

withTiming :: IO a -> IO (Double, a)
withTiming act = do
    TOD s0 f0 <- getClockTime
    x <- act
    TOD s1 f1 <- getClockTime
    let sp = fromIntegral (s1 - s0) + (1.0e-12 * fromIntegral (f1 - f0))
    sp `seq` return (sp, x)

-- | List of values with ping
data SPList a = SPList [(ServerPing,a)] Integer EpochSeconds
toSPList xs to = SPList [(newServerPing, x) | x <- xs] 1 to

withSPList :: Eq a => SPList a -> (a -> IO b) -> IO (SPList a, b)
withSPList = splWork

{-
withSPList (SPList [] _)           _    = fail "withSPList: No entries in SPList"
withSPList (SPList ((osp,c):cs) x) comp = do
    (new,res) <- withTiming $ comp c
    let spl = insertBy compareFst (addSample new osp,c) (mapFst ageServerPing cs)
    spl `seq` return (SPList spl (x+1), res)
-}

mergeSPList :: SPList a -> SPList a -> SPList a
mergeSPList a@(SPList _ ax _) b@(SPList _ bx _) = 
    if ax > bx then a else b

splWork :: Eq a => SPList a -> (a -> IO b) -> IO (SPList a, b)
splWork (SPList [] _ _)  _   = fail "splWork: No entries in SPList"
splWork (SPList xs v to) act = do
    mv <- newMVar Nothing
    let pulsar = sleep 2 >> putMVar mv Nothing >> pulsar
    pt <- fork pulsar
    let loop _   _    0 _   = fail "splWorkLoop: Timeout"
        loop rem tids k acc = let w Nothing = 
                                    if null rem
                                      then loop rem tids (k-1) acc
                                      else do let ((osp,c):cs) = rem
                                              let put (a,b) = putMVar mv $ Just (c,a,b)
                                              tid <- fork (withTiming (act c) >>= put)
                                              loop cs (tid:tids) 20 ((osp,c):acc)
                                  w (Just (cand,tm,res)) = do 
                                    mapM_ killThread tids
                                    let f k (a,b) = (if b == cand then addSample tm a else addSample k a,b)
                                    let ll = zipWith f [1..] acc
                                    let ss = sortFst $ rem ++ ll
                                    ss `seq` return (ss,res)
                              in takeMVar mv >>= w
    (a,b) <- loop xs [] 20 []
    killThread pt
    return (SPList a (v+1) to, b)


sortFst xs = sortBy compareFst xs
compareFst (a,_) (b,_) = compare a b

-- | Cached tree of NS records
data NSTree = NSTree { nsTree :: M.Map String (SPList NameServer) }

instance Show NSTree where
    show (NSTree t) = unlines $ map s $ M.toAscList t
        where s (k,v) = "\n"++k++"\n"++(unlines $ map ("\t"++) $ lines $ ss v)
              ss (SPList lst _ _) = unlines $ map si lst
              si (sp,NameServer n ip) = qd n++" \t "++decIP ip++" \t "++(show $ srtt sp)

-- | The global cache

{-# NOINLINE nsCache #-}
nsCache :: MVar NSTree
nsCache = unsafePerformIO $ do
    nsl <- getNameServers
    mv <- newMVar $ NSTree $ M.singleton "" $ toSPList nsl (-1)
    fork $ killer mv
    return mv

killer mv = do sleep 60
               ctime <- getEpochSeconds
               let f (SPList _ _ v) = v > ctime || v < 0
               modifyMVar_ nsCache (\nst -> return $ nst { nsTree = M.filter f $ nsTree nst })
               killer mv


{- debug utility.
setServerFor :: String -> String -> IO ()
setServerFor sname sip = modifyMVar_ nsCache $ \mt -> 
  return $ mt { nsTree = M.insert sname (toSPList [NameServer (enc sname) (encIP sip)] 1) $ nsTree mt }
-}

-- | Lookup


-- nsL :: Name -> [String]

-- | Perform a DNS lookup using caching.
ns :: Question -> IO Packet
ns q = withGC (nsNC q) q

-- | Perform a DNS lookup without using cache.
nsNC :: Question -> IO Packet
nsNC q = nsNCr q =<< newCounter 20

nsr q c = withGC (nsNCr q c) q

-- | Perform a DNS lookup without using cache.
nsNCr :: Question -> Counter -> IO Packet
nsNCr q@(Q n _ _) counter = do
  let ps  = nameParts n
      e p = fail ("Lookup for: "++show q++" had no results: "++show p)
  nsL counter q ps $ store counter q (head ps) e 

nsL :: Counter -> Question -> [String] -> (Packet -> IO Packet) -> IO Packet
nsL _       _ []     _       = fail "nsL empty parts list"
nsL counter q (p:ps) cont    = do 
    dnsLog ("nsL for "++show q++" at "++show p)
    NSTree m <- readMVar nsCache
    maybe (nsL counter q ps $ store counter q p cont) (look counter q p cont) $ M.lookup p m


store counter q p cont packet = do
    ctime <- getEpochSeconds                        
    nn <- flip concatMapM (nsPQ packet) $ \nsrr -> do
              let n = rrNS nsrr
              ws0 <- getRRAs n (asPQ packet) $ getAC counter
              ws1 <- if null ws0 then getAC counter n else return ws0
              return [ NameServer n w | w <- ws1 ]
    let spl = toSPList nn (ctime + max 120 (packetTTL packet))
    when (null nn) $ fail ("Looking FAIL: "++show packet)
    ins p spl >> look counter q p cont spl

look :: Counter -> Question -> String -> (Packet -> IO Packet) -> SPList NameServer -> IO Packet
look counter q p cont s = do
    decCounter counter $ fail ("Recursion counter reached zero at " ++ show q)
    (s',res) <- withSPList s (\ns -> query q =<< openNameServer (ipNS ns))
    ins p s'
    logM "HAppS.Network.DNS.NSTree" INFO (show q ++ " => " ++ show res)
    let ne f = not $ null $ f res
    case () of
      _ | ne rsPQ                -> checkAnswer q res
        | ne (matching q . asPQ) -> checkAnswer q (res { rsPQ = matching q (asPQ res) })
        | ne nsPQ   -> cont res
        | otherwise -> fail "NSTree: nothing known"

ins p v = modifyMVar_ nsCache $ \nst -> do
            return $ nst { nsTree = M.insertWith mergeSPList p v $ nsTree nst }

matching (Q n t c) = filter (\rr -> n == rname rr && rtype rr == t && rclass rr == c)

----

-- | Find A records for a given hostname.
nsA :: String -> IO [Word32]
nsA = getA . enc

-- | Like nsA but uses a Name for input.
getA :: Name -> IO [Word32]
getA name = newCounter 20 >>= \c -> getA' c name name

getAC counter name = getA' counter name name

getA' :: Counter -> Name -> Name -> IO [Word32]
getA' counter old name = do
    p <- nsr (rquestion name 1 1) counter
    let lst = rsPQ p ++ asPQ p
    getRRAs name lst $ \x -> do
       let loop c = do ws <- getRRAs c lst loop
                       logM "HAppS.Network.DNS.NSTree" INFO ("getA' "++show (c,lst)++" => "++show ws)
                       if null ws then getA' counter old c else return ws
       loop x

getRRAs :: Name -> [RR] -> (Name -> IO [Word32]) -> IO [Word32]
getRRAs name rrs cf = do
     let h rr | rname rr /= name = return []
              | rtype rr == 1    = return [rrA rr]
              | rtype rr == 5    = do let (RRvCName n) = rrval rr
                                      dnsLog ("> CNAME: "++qd name++" -> "++qd n)
                                      cf n
              | otherwise        = return []
     concatMapM h rrs

rrA rr = let (RRvA w) = rrval rr in w
rrNS rr = let (RRvNS x) = rrval rr in x

-- | Debugging, shows the nameserver cache.
prettyNSTree :: IO String
prettyNSTree = fmap show $ readMVar nsCache

concatMapM f xs = (return . concat) =<< mapM f xs

checkAnswer _ x = return x

-- Counters

type Counter = MVar Int

newCounter      = newMVar
decCounter :: Counter -> IO Int -> IO ()
decCounter mv f = modifyMVar_ mv (\x -> if x == 0 then f else return (x-1))

