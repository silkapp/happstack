-----------------------------------------------------------------------------
-- |
-- Module      :  Happstack.Util.Common
-- Copyright   :  (c) Happstack.com, 2009; (c) HAppS.org, 2005
-- License     :  BSD3
-- 
--
-- Various helper routines.
-----------------------------------------------------------------------------
module Happstack.Util.Common where

import System.Log.Logger
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Char8 as P
import Data.Char
import Data.Int
import System.IO
import System.Exit
import System.IO.Error
import System.Process
import System.IO.Unsafe
import System.Time

type Seconds = Int
type EpochSeconds = Int64
epochSeconds :: CalendarTime -> EpochSeconds
epochSeconds ct = let TOD sec _ = toClockTime ct in fromIntegral sec
eSecsToCalTime :: EpochSeconds -> IO CalendarTime
eSecsToCalTime s = toCalendarTime (TOD (fromIntegral s) 0)
epochPico :: CalendarTime -> Integer
epochPico ct = fromIntegral (epochSeconds ct) * 1000

----reliable getline and putline

logMC :: Priority -> String -> IO ()
logMC = logM "Happstack.Util.Common"

-- | Put a line into a handle followed by "\r\n" and echo to stdout
hPutLine :: Handle -> String -> IO ()
hPutLine handle line = 
	do
	hPutStr handle $ line
	hPutStr handle "\r\n"
	hFlush handle
	logMC DEBUG line
	return ()

-- | Get a line from the handle and echo to stdout
hGetLn :: Handle -> IO String
hGetLn handle = do
    let hGetLn' = do
          c <- hGetChar handle
          case c of
	    '\n' -> return []
            '\r' -> do c2 <- hGetChar handle 
		       if c2 == '\n' then return [] else getRest c
	    _    -> getRest c
	getRest c = fmap (c:) hGetLn'
    line <- hGetLn'
    logMC DEBUG line
    return line


unBracket, ltrim, rtrim, trim :: String -> String
unBracket = tail . init . trim

ltrim = dropWhile isSpace

rtrim = reverse.ltrim.reverse
trim=ltrim.rtrim

splitList :: Eq a => a -> [a] -> [[a]]
splitList _   [] = []
splitList sep list = first:splitList sep rest
	where (first,rest)=split (==sep) list

splitListBy :: (a -> Bool) -> [a] -> [[a]]
splitListBy _ [] = []
splitListBy f list = first:splitListBy f rest
	where (first,rest)=split f list

-- | Split is like break, but the matching element is dropped.
split :: (a -> Bool) -> [a] -> ([a], [a])
split f s = (left,right)
	where
	(left,right')=break f s
	right = if null right' then [] else tail right'
							

-- | Read file with a default value if the file does not exist.
mbReadFile :: a -> (String -> a) -> FilePath -> IO a
mbReadFile noth just path  = 
	(do text <- readFile path;return $ just text)
	`catch` \err -> if isDoesNotExistError err then return noth else ioError err

doSnd :: (a -> b) -> (c,a) -> (c,b)
doSnd f (x,y) = (x,f y)

doFst :: (a -> b) -> (a,c) -> (b,c)
doFst f (x,y) = (f x,y)


mapFst :: (a -> b) -> [(a,x)] -> [(b,x)]
mapFst f = map (\ (x,y)->(f x,y)) 
mapSnd :: (a -> b) -> [(x,a)] -> [(x,b)]
mapSnd f = map (\ (x,y)->(x,f y)) 

revmap :: a -> [a -> b] -> [b]
revmap item = map (\f->f item)

comp :: Ord t => (a -> t) -> a -> a -> Ordering
comp f e1 e2 = f e1 `compare` f e2

-- | Run an external command. Upon failure print status
--   to stderr.
runCommand :: String -> [String] -> IO ()
runCommand cmd args = do 
    (_, outP, errP, pid) <- runInteractiveProcess cmd args Nothing Nothing
    let pGetContents h = do mv <- newEmptyMVar
                            let put [] = putMVar mv []
                                put xs = last xs `seq` putMVar mv xs
                            forkIO (hGetContents h >>= put)
                            takeMVar mv
    os <- pGetContents outP
    es <- pGetContents errP
    ec <- waitForProcess pid
    case ec of
      ExitSuccess   -> return ()
      ExitFailure e ->
          do hPutStrLn stderr ("Running process "++unwords (cmd:args)++" FAILED ("++show e++")")
             hPutStrLn stderr os
             hPutStrLn stderr es
             hPutStrLn stderr ("Raising error...")
             fail "Running external command failed"


-- | Unsafe tracing, outputs the message and the value to stderr.
debug :: Show a => String -> a -> a
debug msg s = 
    seq (unsafePerformIO (hPutStr stderr ("DEBUG: "++msg++"\n") >> 
                                  hPutStr stderr (show s++"\n"))) s

{-# NOINLINE debugM #-}
-- | Unsafe tracing messages inside a monad.
debugM :: Monad m => String -> m ()
debugM msg = unsafePerformIO (P.hPutStr stderr (P.pack (msg++"\n")) >> hFlush stderr) `seq` return ()

-- | Read in any monad.
readM :: (Monad m, Read t) => String -> m t
readM s = case readsPrec 0 s of
            [(v,"")] -> return v
            _        -> fail "readM: parse error"

-- | Convert Maybe into an another monad.
maybeM :: Monad m => Maybe a -> m a
maybeM (Just x) = return x
maybeM _        = fail "maybeM: Nothing"

-- ! Convert Bool into another monad
boolM :: (MonadPlus m) => Bool -> m Bool
boolM False = mzero
boolM True  = return True

notMb :: a-> Maybe a-> Maybe a
notMb v1 v2 = maybe (Just v1) (const Nothing) $ v2

periodic :: [Int] -> IO () -> IO ThreadId
periodic ts = forkIO . periodic' ts

-- a little something to fix the types of ^
infixr 8 .^
(.^) :: Int->Int->Int
a .^ b = a ^ b
periodic' :: [Int] -> IO a -> IO a
periodic' [] x = x
periodic' (t:ts) x = x >> threadDelay ((10 .^ 6)*t) >> periodic' ts x
