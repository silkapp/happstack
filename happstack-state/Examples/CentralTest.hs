{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable, TypeFamilies,
             OverlappingInstances, UndecidableInstances #-}

import Control.Exception
import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.State
import Data.Generics
import Happstack.Data
import Happstack.State
import System.Directory
import Control.Applicative
import System.Environment
import System.IO
import System.Exit

data MyState = MyState { myInt    :: Int,
                         myString :: String}
    deriving (Show,Read,Typeable,Data)

$(deriveNewData [''MyState])

instance Version MyState
instance Serialize MyState where
    getCopy = contain $ MyState <$> safeGet <*> safeGet
    putCopy (MyState i s) = contain $ safePut i >> safePut s

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

sync :: Update MyState ()
sync = return ()

$(mkMethods ''MyState ['getMyData, 'setMyData, 'getMyInt, 'setMyInt, 'sync])

instance Component MyState where
    type Dependencies MyState = End
    initialValue = MyState 0 ""

test :: IO ()
test = do print =<< query GetMyData
          update $ SetMyData 10 "Foo"
          print =<< query GetMyData
          update $ SetMyData 33 "Bar"
          print =<< query GetMyData
          print =<< query GetMyInt
          update $ SetMyInt 18
          print =<< query GetMyData

main :: IO ()
main = processLoggingFlags $
       do args <- getArgs
          case args of
            (app_name : rest) -> do control <- startSystemStateAmazon app_name (Proxy :: Proxy MyState)
                                    update $ Sync
                                    case rest of
                                      ["get"]
                                        -> do print =<< query GetMyData
                                      ["set", int, string]
                                        -> do update $ SetMyData (read int) string
                                      ["checkpoint"]
                                        -> do createCheckpoint control
                                      _
                                        -> do printUsage
                                     `finally` shutdownSystem control
            _ -> printUsage

printUsage
    = do prog <- getProgName
         hPutStrLn stderr $ "Usage:"
         hPutStrLn stderr $ "  " ++ prog ++ " APPLICATION_NAME get"
         hPutStrLn stderr $ "  " ++ prog ++ " APPLICATION_NAME set INT STRING"
         hPutStrLn stderr $ "  " ++ prog ++ " APPLICATION_NAME checkpoint"
         hPutStrLn stderr $ "\nFor detailed output, append '--log-level=DEBUG'."
         exitWith (ExitFailure 1)

