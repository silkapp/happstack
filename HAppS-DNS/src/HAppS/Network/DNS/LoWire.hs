module HAppS.Network.DNS.LoWire 
    (getW16, getW32, getW64, getW8Lst, getW16Lst, getName,
     putW16, putW32, putW64, putW8Lst, putW16Lst, putName,
     htonl, ntohl, htons, ntohs
    ) where

import HAppS.Network.DNS.MutableEnv
import HAppS.Network.DNS.Type
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BB
import qualified Data.ByteString.Unsafe as BB
import Foreign hiding (newArray)
import Foreign.C.String

ntohs = htons
ntohl = htonl

-- | Convert a 'Word16' between native and network byte order.
htons :: Word16 -> Word16
-- | Convert a 'Word32' between native and network byte order.
htonl :: Word32 -> Word32
(htons,htonl) = if bige then (id,id) else (c16,c32)
    where c16 :: Word16 -> Word16
          c16 v = (v `shiftR` 8) + ((v .&. 0xFF) `shiftL` 8)
          c32 v = ( v                 `shiftR` 24) +
                  ((v .&. 0x000000FF) `shiftL` 24) +
                  ((v .&. 0x0000FF00) `shiftL` 8) +
                  ((v .&. 0x00FF0000) `shiftR` 8)
          bige  = unsafePerformIO $ do let x :: Word32
                                           x = 0x12345678
                                       s <- with x $ \ptr -> peekCStringLen (castPtr ptr,4)
                                       case s of
                                         "\x12\x34\x56\x78" -> return True
                                         "\x78\x56\x34\x12" -> return False
                                         _                  -> error "Testing endianess failed"

pState :: String -> MutEnv ()
pState = const $ return ()
-- pState s = do { p <- showME; liftIO $ putStrLn (s++" "++p) }

getW8Lst :: Int -> MutEnv [Word8]
getW8Lst n = do p <- advance n
		liftIO $ peekArray n (castPtr p)
	      
getW16Lst :: Int -> MutEnv [Word16]
getW16Lst n = do p <- advance (n * 2)
		 lst <- liftIO $ peekArray n (castPtr p)
		 return $ map ntohs lst

getName :: MutEnv Name
getName = do pState "getName"
	     nl <- getNamE 10
	     return $ BS.pack nl

getNamE :: Int -> MutEnv [Word8]
getNamE 0 = lift $ fail " getNamE loop"
getNamE n = do pState " getNamE"
               ptr <- advance 1
	       x <- liftIO $ (peek ptr >>= return . fromIntegral)
	       case x of
		0          -> do return [0]
		_ | x < 64 -> do seg   <- getW8Lst x
				 rest  <- getNamE n
				 return $ fromIntegral x : seg++rest
		  | True   -> do advance 1
                                 nextUM <- liftIO (peek (castPtr ptr))
				 let x' = fromEnum (ntohs nextUM .&. 16383)
				 pState ("  name comp, x = "++show x')
                                 r <- withGoto x' (getNamE (n-1))
                                 pState ("  after goto")
                                 return r
                                 
   

getW32 :: MutEnv Word32
getW32 = do ptr <- advance 4
	    w32 <- liftIO $ peek (castPtr ptr)
	    return (ntohl w32)

getW16 :: MutEnv Word16
getW16 = do ptr <- advance 2
	    w16 <- liftIO $ peek (castPtr ptr)
	    return (ntohs w16)


getW64 :: MutEnv Word64
getW64 = do w1 <- getW32
	    w2 <- getW32
	    return $ (fromIntegral w1 `shiftL` 32) + fromIntegral w2 

-- * write routines - safe

putW16 :: Word16 -> MutEnv ()
putW16 w = do p <- advance 2
              liftIO (poke (castPtr p) (htons w))

putW32 :: Word32 -> MutEnv ()
putW32 w = do p <- advance 4
              liftIO (poke (castPtr p) (htonl w))

putW64 :: Word64 -> MutEnv ()
putW64 w = do putW32 $ fromIntegral $ w `shiftR` 32
	      putW32 $ fromIntegral $ w .&. 0xFFFFFFFF

putW16Lst :: [Word16] -> MutEnv ()
putW16Lst lst = do p <- advance (2*length lst)
                   liftIO (pokeArray (castPtr p) (map htons lst))

putW8Lst :: [Word8] -> MutEnv ()
putW8Lst lst = do p <- advance (length lst)
                  liftIO $ pokeArray p lst

putName :: Name -> MutEnv ()
putName n = do
  let len = BS.length n
  dest <- advance len
  liftIO $ BB.unsafeUseAsCString n $ \src -> BB.memcpy dest (castPtr src) (fromIntegral len)
