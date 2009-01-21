{-# OPTIONS -fglasgow-exts -fth #-}
module Test where

import HAppS.State
import HAppS.State.ComponentTH    ( mkMethods )

import Control.Exception          ( bracket )
import Control.Concurrent

import Data.Dynamic
import Data.Maybe                 ( fromMaybe )
import Control.Monad              ( liftM, forever, forM_ )
import Control.Monad.State        ( put, get )
import Control.Monad.Reader       ( ask )

import System.IO

data Test = Test { unTest :: [(String,String)] } deriving (Typeable)

instance Version Test
$(deriveSerialize ''Test)


addMsg :: String -> String -> Update Test ()
addMsg usr msg
    = do Test msgs <- get
         put $ Test $ (usr,msg):msgs

getHistory :: Query Test [(String,String)]
getHistory = do Test msgs <- ask
                return $ take 10 msgs

$(mkMethods ''Test [ 'addMsg
                   , 'getHistory ])

instance Component Test where
    type Dependencies Test = End -- No dependencies.
    initialValue = Test []

rootState :: Proxy Test
rootState = Proxy

test :: IO ()
test = bracket (startSystemState rootState) closeTxControl $ \ctl ->
       do hSetBuffering stdout LineBuffering
          getEvent <- getEventStream
          forkIO $ forever $ do event <- getEvent
                                case fromDynamic (eventData event) of
                                  Nothing
                                      -> putStrLn $ "Unknown event: " ++ show (eventData event)
                                  Just (AddMsg usr msg)
                                      -> putStrLn $ usr ++ ": " ++ msg
          hist <- query $ GetHistory
          putStrLn "History"
          forM_ hist $ \(usr,msg) ->
              putStrLn $ "  " ++ usr ++ ": " ++ msg
          putStrLn "History end"
          update $ AddMsg "Lemmih" "Hello world"
          update $ AddMsg "Lemmih" "Testing event sniffing"
          createCheckpoint ctl
          return ()
