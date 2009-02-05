module Happstack.State.Saver.Impl.Memory where

import Happstack.Data.Serialize
import Happstack.State.Saver.Types

import Control.Concurrent

import qualified Data.ByteString.Lazy.Char8 as L
import Data.Maybe

import qualified Data.Map as M

type Store = M.Map String (M.Map Int L.ByteString)

newMemoryStore :: IO (MVar Store)
newMemoryStore = newMVar M.empty

memoryReader :: Serialize a => MVar Store -> String -> Int -> IO (ReaderStream a)
memoryReader store key cutoff
    = do return $ ReaderStream
                    { readerClose = do return ()
                    , readerGet   = withMVar store $ \storeData ->
                                    let getAllData ((cut,_):xs) n | n > cut = getAllData xs n
                                        getAllData ((cut,dat):xs) n | n == cut = dat : getAllData xs (n+1)
                                        getAllData _ _ = []
                                        allData = getAllData (maybe [] M.toList $ M.lookup key storeData) cutoff
                                    in return $ (parseAll (L.concat allData), length allData)
                    , readerGetUncut = withMVar store $ \storeData ->
                                       return $ parseAll $ fromMaybe L.empty $ M.lookup cutoff =<< M.lookup key storeData
                    }

memoryWriter :: Serialize a => MVar Store -> String -> Int -> IO (WriterStream a)
memoryWriter store key cutoffInit
    = do cutoffVar <- newMVar cutoffInit
         modifyMVar_ store $ \storeData -> return (addToStore key cutoffInit L.empty storeData)
         return $ WriterStream
                    { writerClose = return ()
                    , writerAdd   = \m f ->
                                    do cutoff <- readMVar cutoffVar
                                       modifyMVar_ store $ \storeData -> return $ addToStore key cutoff (serialize m) storeData
                                       forkIO f
                                       return ()
                    , writerAtomicReplace = \ss -> do cutoff <- readMVar cutoffVar
                                                      modifyMVar_ store $ \storeData -> return $ setStore key cutoff (serialize ss) storeData
                    , writerCut = do newCut <- modifyMVar cutoffVar $ \cutoff -> return (cutoff+1, cutoff+1)
                                     modifyMVar_ store $ \storeData -> return (addToStore key newCut L.empty storeData)
                                     return newCut
                    }

parseAll :: Serialize a => L.ByteString -> [a]
parseAll = loop
    where loop l | L.null l = []
          loop l = let (a,rest) = deserialize l
                   in a:loop rest

addToStore :: (Ord a, Ord b) => a -> b -> L.ByteString
                         -> M.Map a (M.Map b L.ByteString)
                         -> M.Map a (M.Map b L.ByteString)
addToStore key cutoff val store
    = M.unionWith (M.unionWith L.append) store element
   where element = M.singleton key $ M.singleton cutoff val

setStore :: (Ord a) => a -> b -> c
                       -> M.Map a (M.Map b c)
                       -> M.Map a (M.Map b c)
setStore key cutoff val store
    = M.unionWith (\_ _ -> M.singleton cutoff val) store element
   where element = M.singleton key $ M.singleton cutoff val
