{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             OverlappingInstances, UndecidableInstances #-}
{-
import Control.Exception
import Control.Monad.Reader
import Control.Monad.State
import Data.Generics
import HAppS.Data
import HAppS.State
import System.Directory

data MyState = MyState { myInt    :: Int,
                         myString :: String}
    deriving (Show,Read,Typeable,Data)

$(deriveNewData [''MyState])

getMyData :: MonadReader MyState m => m (Int, String)
getMyData = do st <- ask
               return (myInt st, myString st)

setMyData :: MonadState MyState m => Int -> String -> m ()
setMyData i s = put $ MyState { myInt = i, myString = s }

getMyInt :: MonadReader MyState m => m Int
getMyInt = do st <- ask
              return (myInt st)

setMyInt :: MonadState MyState m => Int -> m ()
setMyInt i = do st <- get
                put $ st { myInt = i }

$(methods ''MyState ['getMyData, 'setMyData, 'getMyInt, 'setMyInt])
$(atStart ''MyState [])

clean :: IO ()
clean = do try (removeDirectoryRecursive "_local")
           return ()

test :: IO ()
test = do print =<< query GetMyData
          update $ SetMyData 10 "Foo"
          print =<< query GetMyData
          update $ SetMyData 33 "Bar"
          print =<< query GetMyData
          print =<< query GetMyInt
          update $ SetMyInt 18
          print =<< query GetMyData

$(systemState ''MyState)

main :: IO ()
main = do clean
          control <- startSystemState
          _ <- getEventStream
          test
          stopSystemState control
          clean
-}
main = return ()
