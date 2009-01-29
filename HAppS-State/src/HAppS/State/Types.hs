{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE CPP, UndecidableInstances, TemplateHaskell #-}
module HAppS.State.Types where

import Control.Concurrent.STM
import Data.Int
import Data.Word
import qualified GHC.Conc(unsafeIOToSTM)
import System.Random -- (StdGen)

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Trans
import Data.Generics
-- Monad things


data Env = Env
    { evRandoms :: TVar StdGen
    , evContext :: TxContext }

type TxId      = Int64
type EpochMilli= Int64

instance Typeable StdGen where typeOf _ = mkTyConApp (mkTyCon "System.Random.StdGen") []

instance Random Word64 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

instance Random Int64 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

integralRandomR :: (Integral a, RandomGen g) => (a,a) -> g -> (a,g)
integralRandomR  (a,b) g = case randomR (fromIntegral a :: Integer,
                                         fromIntegral b :: Integer) g of
                            (x,g') -> (fromIntegral x, g')


data TxContext = TxContext
    { txId     :: TxId,
      txRand   :: Word64,
      txTime   :: EpochMilli,
      txStdGen :: StdGen
    }  deriving (Read,Show,Typeable)


{-
  Is STM really be best backend monad?
  We don't use any of the STM features.
-}
-- | ACID computations that work with any state and event types.
type AnyEv a = forall t. (Monad (t STM), MonadTrans t) => Ev (t STM) a

-- | Monad for ACID event handlers.
newtype Ev m t = Ev { unEv :: Env -> m t }

instance (Typeable st, Typeable1 m) => Typeable1 (ReaderT st m) where
    typeOf1 x = mkTyConApp (mkTyCon "Control.Monad.Reader.ReaderT") [typeOf (undefined :: st), typeOf1 (m x)]
        where m :: ReaderT st m a -> m a
              m = undefined

instance (Typeable st, Typeable1 m) => Typeable1 (StateT st m) where
    typeOf1 x = mkTyConApp (mkTyCon "Control.Monad.State.StateT") [typeOf (undefined :: st), typeOf1 (m x)]
        where m :: StateT st m a -> m a
              m = undefined

instance (Typeable state, Typeable t) => Typeable (Ev (ReaderT state STM) t) where
    typeOf (Ev _cmd) = mkTyConApp (mkTyCon "HAppS.State.Types.Ev") [typeOf (u::ReaderT state STM t)]
        where u = undefined
instance (Typeable state, Typeable t) => Typeable (Ev (StateT state STM) t) where
    typeOf (Ev _cmd) = mkTyConApp (mkTyCon "HAppS.State.Types.Ev") [typeOf (u::StateT state STM t)]
        where u = undefined

type Query state = Ev (ReaderT state STM)
type Update state = Ev (StateT state STM)

-- unsafe lifting

unsafeIOToEv :: IO a -> AnyEv a
unsafeIOToEv c = unsafeSTMToEv (unsafeIOToSTM c)
unsafeSTMToEv :: STM a -> AnyEv a
unsafeSTMToEv c = Ev $ \_ -> lift c
unsafeIOToSTM :: IO a -> STM a
unsafeIOToSTM = GHC.Conc.unsafeIOToSTM



-- Misc

newtype Shadow t a = Shadow { unShadow :: a }  deriving Typeable

newtype UsingXml a = UsingXml { unXml :: a } deriving Typeable

