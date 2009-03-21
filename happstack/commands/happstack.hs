-- happstack command
module Main where

import Control.Monad (filterM, liftM, mapM, mapM_)
import Paths_happstack (getDataDir)
import System.Environment (getArgs)
import System.Directory
    ( canonicalizePath
    , createDirectoryIfMissing
    , doesFileExist
    , doesDirectoryExist
    , copyFile
    )
import System.FilePath ((</>), makeRelative)
import System.FilePath.Find (always, find)

data Command
  = NewProjectCmd FilePath
  | HelpCmd

main :: IO ()
main = do
  args <- getArgs
  case (processArgs args) of
    NewProjectCmd dir -> do
      newProject dir
    HelpCmd -> do
      putStrLn "Usage: happstack <command>"
      putStrLn "Possible commands:"
      putStrLn "  new project <dir>: create a new happstack project"
  
processArgs :: [String] -> Command
processArgs ["new", "project", dir] = NewProjectCmd dir
processArgs ["help"]                = HelpCmd
processArgs _                       = HelpCmd
    
newProject :: FilePath -> IO ()
newProject destDir = do
    dataDir <- liftM (</> "templates" </> "project") getDataDir

    -- create destDir if needed
    createDirectoryIfMissing True destDir
  
    -- create dirs
    srcDirs <- getSourceDirs dataDir
    let destDirs = map ((destDir </>) . makeRelative dataDir) srcDirs
    mapM_ (createDirectoryIfMissing False) destDirs

    -- create files
    srcFiles <- getSourceFiles dataDir
    let destFiles = map ((destDir </>) . makeRelative dataDir) srcFiles
    mapM_ cp $ zip srcFiles destFiles
      
-- only source dirs
getSourceDirs :: FilePath -> IO [FilePath]
getSourceDirs dataDir = filterM doesDirectoryExist =<< getSourceData dataDir

-- only source files
getSourceFiles :: FilePath -> IO [FilePath]
getSourceFiles dataDir = filterM doesFileExist =<< getSourceData dataDir

-- source data for project skeleton
getSourceData :: FilePath -> IO [FilePath]
getSourceData rawDataDir = do
    dataDir <- canonicalizePath rawDataDir
    all <- mapM canonicalizePath =<< (find always always dataDir)
    return $ filter (/= dataDir) all

-- does not work for folders
cp :: (FilePath, FilePath) -> IO ()
cp (src, dest) = do
  exists <- doesFileExist dest
  if exists
    then putStrLn $ "File exists already, skipping: " ++ dest
    else copyFile src dest

