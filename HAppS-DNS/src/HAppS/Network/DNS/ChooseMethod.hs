-- See the main DNS.hs for documentation.
module HAppS.Network.DNS.ChooseMethod
    (NameServer(..), getNameServers, initDNS, dnsLog
    ) where

import HAppS.Network.DNS.Type

import Control.Monad
import Data.IORef
import Data.Word
import System.Environment(getEnv)
import System.IO.Unsafe

data NameServer = NameServer 
    { nameNS :: Name,
      ipNS   :: Word32
    } deriving(Eq)

instance Show NameServer where
    show ns = unwords ["NameServer",qd $ nameNS ns,"@",decIP $ ipNS ns]


-- Initialize DNS with non-standard defaults.
initDNS :: Maybe (String -> IO ()) -- ^ Logging function
        -> Maybe [String]          -- ^ List of nameserver IP addresses for root-servers to use
        -> IO ()
initDNS lf grs = do
  maybe (return ()) (writeIORef lref) lf
  maybe (return ()) (writeIORef sref . return) grs



{-# NOINLINE lref #-}
lref :: IORef (String -> IO ())
lref = unsafePerformIO $ newIORef putStrLn

dnsLog x = readIORef lref >>= \f -> f x

{-# NOINLINE sref #-}
sref :: IORef (IO [String])
sref = unsafePerformIO $ newIORef getRawServers

-- | Get a list of name servers.
getNameServers = return . map w =<< join (readIORef sref)
    where w x = NameServer (enc x) (encIP x)

getRawServers :: IO [String]
getRawServers = flip catch (\_ -> return roots) $ do
    ev <- getEnv "NAMESERVER"
    case ev of
      "resolv" -> fmap parseResolv $ readFile "/etc/resolv.conf"
      _        -> return $ words ev

parseResolv :: String -> [String]
parseResolv inp = do
    line <- lines inp
    case words line of
      ["nameserver",x] -> return x
      _                -> fail ""

-- Root addresses
roots = ["198.41.0.4",     -- A
         "192.228.79.201", -- B
         "192.33.4.12",    -- C
         "128.8.10.90",    -- D
         "192.203.230.10", -- E
         "192.5.5.241",    -- F
         "192.112.36.4",   -- G
         "128.63.2.53",    -- H
         "192.36.148.17",  -- I
         "192.58.128.30",  -- J
         "193.0.14.129",   -- K
         "198.32.64.12",   -- L
         "202.12.27.33"    -- M
        ]
