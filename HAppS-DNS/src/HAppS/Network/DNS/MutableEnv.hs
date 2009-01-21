module HAppS.Network.DNS.MutableEnv (MutEnv, runMutableEnv, advance, withGoto, showME) where
-- Offer a mutable environment

import Control.Monad.State 
import Data.Word (Word8)
import Foreign.Ptr (Ptr, plusPtr)

-- | The mutable buffer parsing monad.
type MutEnv = StateT FState IO
type CPtr = Ptr Word8

--                   Current Length Start
data FState = FState !Int    !Int   !CPtr

showME :: MutEnv String
showME = do FState cur len start <- get
            return $ unwords ["current",show cur,"length",show len,"ptr",show start]

-- | Run a computation with the specified buffer.
runMutableEnv :: CPtr -> Int -> MutEnv a -> IO a
runMutableEnv ptr iv comp = evalStateT comp (FState 0 iv ptr)

-- | Return the current position and advance the position by n bytes.
-- Fails when there is insufficient space in the buffer.
advance :: Int -> MutEnv (Ptr Word8)
advance n = do FState c l s <- get
               when (c+n > l) (fail "buffer overflow")
               put (FState (c+n) l s)
               return (plusPtr s c)

-- | Goto absolute offset n and do the computation there.
withGoto :: Int -> MutEnv a -> MutEnv a
withGoto n f = do (FState _ l s) <- get
                  liftIO $ evalStateT f (FState n l s)
