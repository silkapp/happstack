{-# OPTIONS -fglasgow-exts -fth #-}
module Test where

import HAppS.State
import HAppS.State.ComponentTH    ( mkMethods )

import Control.Exception          ( bracket )

import Data.Typeable              ( Typeable )
import Data.Maybe                 ( fromMaybe )
import Control.Monad              ( liftM )
import Control.Monad.State        ( put )
import Control.Monad.Reader       ( ask )

-- Simple mutable cell.
data Test a = Test { unTest :: Maybe a } deriving (Typeable)

instance Version (Test a) -- use class defaults
$(deriveSerialize ''Test)


setTest :: a -> Update (Test a) ()
setTest a = put (Test (Just a))

getTest :: a -> Query (Test a) a
getTest def
    = do Test m <- ask
         return $ fromMaybe def m

$(mkMethods ''Test [ 'setTest
                   , 'getTest ])

instance (Serialize a) => Component (Test a) where
    type Dependencies (Test a) = End -- No dependencies.
    initialValue = Test Nothing

rootState :: Proxy (Test Int)
rootState = Proxy

test :: IO ()
test = bracket (startSystemState rootState) closeTxControl $ \ctl ->
       do n <- query $ GetTest 0
          update (SetTest (n+1 :: Int))
          print =<< query (GetTest (0 :: Int))
          createCheckpoint ctl -- Checkpoints are just an optimization.
                               -- Removing the line should give the same results.
          return ()
