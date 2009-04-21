-- happstack command
module Main (main) where

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
import Happstack.Util.AutoBuild (autoBuild)
import Happstack.Util.FileManip (always, find)

data Command
  = NewProjectCmd FilePath
  | BuildAutoCmd String String [String]
  | HelpCmd

main :: IO ()
main = do
  args <- getArgs
  case processArgs args of
    NewProjectCmd dir -> newProject dir
    BuildAutoCmd buildCmd binPath binArgs -> buildAuto buildCmd binPath binArgs
    HelpCmd -> do
      putStrLn "Usage: happstack <command>"
      putStrLn "Possible commands:"
      putStrLn "  build auto <buildCmd> <binPath> <binArgs>...: invoke the auto-builder"
      putStrLn "  new project <dir>: create a new happstack project"
  
processArgs :: [String] -> Command
processArgs ("build" : "auto" : buildCmd : binPath : binArgs) =
    BuildAutoCmd buildCmd binPath binArgs
processArgs ["new", "project", dir] = NewProjectCmd dir
processArgs ["help"]                = HelpCmd
processArgs _                       = HelpCmd
    
buildAuto :: String -> String -> [String] -> IO ()
buildAuto = autoBuild

newProject :: FilePath -> IO ()
newProject destDir' = do
    dataDir <- liftM (</> "templates" </> "project") getDataDir
    destDir <- canonicalizePath destDir'
    
    -- create destDir if needed
    createDir destDir

    -- create dirs
    srcDirs <- getSourceDirs dataDir
    let destDirs = map ((destDir </>) . makeRelative dataDir) srcDirs
    mapM_ createDir destDirs

    -- create files
    srcFiles <- getSourceFiles dataDir
    let destFiles = map ((destDir </>) . makeRelative dataDir) srcFiles
    mapM_ cp $ zip srcFiles destFiles
    where createDir = createDirectoryIfMissing True
      
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

