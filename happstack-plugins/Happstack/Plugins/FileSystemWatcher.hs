-- | Api for watching and notifying file changes.
-- Currently, this is based on inotify, but people may want to
-- provide implementations of this module for non-inotify-supported 
-- platforms.
module Happstack.Plugins.FileSystemWatcher
          ( FSWatcher
          , FSWatchDescriptor
          , initFSWatcher
          , addWatch
          , removeWatch
          ) where

import Control.Concurrent.MVar    (MVar,readMVar,modifyMVar,modifyMVar_,newMVar)
import System.INotify              (INotify, WatchDescriptor, Event(..), EventVariety(..),initINotify)
import qualified System.INotify as I(addWatch, removeWatch)
import System.FilePath             (splitFileName)
import qualified Data.Map       as Map
import           Data.Map          (Map)

-- Keeps watching a file even after it has been deleted and created again.
--
-- It does so by observing the folder which contains the file. When no files
-- are observed in a given folder, the folder stops being observed.
data FSWatcher = FSWatcher 
         INotify                       -- INotify handle
         (MVar 
           (Map FilePath                 -- Folder containing the file
                ( WatchDescriptor        -- Watch descriptor of the folder
                , Map String             -- File being observed
                      (Event -> IO ())   -- Handler to run on file events
                )
           )
         )

data FSWatchDescriptor = FSWatchDescriptor FSWatcher FilePath

-- | Initializes a watcher.
initFSWatcher :: IO FSWatcher
initFSWatcher = do
  iN <- initINotify
  fmvar <- newMVar Map.empty
  return$ FSWatcher iN fmvar

-- Replacement for splitFileName which returns "." instead of an empty folder.
splitFileName' :: FilePath -> (FilePath,String)
splitFileName' fp =
   let (d,f) = splitFileName fp
    in (if null d then "." else d,f)

-- | Runs the callback IO action on modifications to the file at the given path.
--
-- Each file can have only one callback IO action. Registering a new IO action
-- discards any previously registered callback.
--
-- The returned FSWatchDescriptor value can be used to stop watching the file.
addWatch :: FSWatcher -> FilePath -> IO () -> IO FSWatchDescriptor
addWatch piN@(FSWatcher iN fmvar) fp hdl = 
   let (d,f) = splitFileName' fp
    in modifyMVar fmvar$ \fm ->
   case Map.lookup d fm of
     Nothing -> do
         wd <- I.addWatch iN [Modify, Move, Delete] d $ \e -> do 
                  case e of
                     Ignored -> return ()
                     Deleted { filePath = f' } -> callHandler e d f'
                     MovedIn { filePath = f' } -> callHandler e d f'
                     Modified { maybeFilePath = Just f' } -> callHandler e d f'
                     _ -> return ()
         return ( Map.insert d (wd,Map.singleton f (const hdl)) fm 
                , FSWatchDescriptor piN fp 
                )
     Just (wd,ffm) -> return ( Map.insert d (wd,Map.insert f (const hdl) ffm) fm
                             , FSWatchDescriptor piN fp
                             )
  where
     callHandler e d f = do 
       fm <- readMVar fmvar 
       case Map.lookup d fm of 
         Nothing -> return ()
         Just (_,ffm) -> case Map.lookup f ffm of
                           Nothing -> return ()
                           Just mhdl -> mhdl e
 
-- | Stops watching the file associated to the given file descriptor.
removeWatch :: FSWatchDescriptor -> IO ()
removeWatch (FSWatchDescriptor (FSWatcher _iN fmvar) fp) =
   let (d,f) = splitFileName' fp
    in modifyMVar_ fmvar$ \fm ->
   case Map.lookup d fm of
     Nothing -> error$ "removeWatchP: invalid handle for file "++fp
     Just (wd,ffm) -> let ffm' = Map.delete f ffm
                       in if Map.null ffm' then I.removeWatch wd >> return (Map.delete d fm)
                            else return (Map.insert d (wd,ffm') fm)
  
 
