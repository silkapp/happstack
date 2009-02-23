-- happstack command
module Main where

import Paths_happstack (getDataDir)
import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing, doesFileExist, copyFile)
import System.FilePath ((</>))
import Control.Monad (mapM_)

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
  dataDir <- getDataDir
  
  -- create folders
  mapM_ (createDirectoryIfMissing False) (destinationFolders destDir)
  
  -- copy files
  mapM_ cp $ zip (sourceFiles dataDir) (destinationFiles destDir)
      
-- destination folders that need to be created
destinationFolders destDir =
  map
    (destDir </>) 
    [ "bin"
    , "dist"
    , "public"
    , "public/theme"
    , "public/theme/images"
    , "src"
    , "templates"
    ]
  
-- source files for project skeleton (needs to know dataDir location)
sourceFiles dataDir =
  map (dataDir </>) projectFiles

-- destination files for project skeleton
destinationFiles destDir =
  map ((destDir </>) . removePrefix) projectFiles
  where removePrefix = drop $ length "templates/project/" 

-- all files in the data dir
projectFiles = 
  [ "templates/project/bin/build.sh"
  , "templates/project/bin/run-interactive.sh"
  , "templates/project/bin/run.sh"
  , "templates/project/guestbook.cabal"
  , "templates/project/public/theme/images/blockquote.gif"
  , "templates/project/public/theme/images/date.gif"
  , "templates/project/public/theme/images/entrymeta.gif"
  , "templates/project/public/theme/images/grunge.gif"
  , "templates/project/public/theme/images/header_loop.gif"
  , "templates/project/public/theme/images/logo.gif"
  , "templates/project/public/theme/images/menu_hili.gif"
  , "templates/project/public/theme/images/menu_hover.gif"
  , "templates/project/public/theme/images/peel.gif"
  , "templates/project/public/theme/images/ql_rss.gif"
  , "templates/project/public/theme/readme.txt"
  , "templates/project/public/theme/style.css"
  , "templates/project/Setup.hs"
  , "templates/project/src/AppControl.hs"
  , "templates/project/src/AppLogger.hs"
  , "templates/project/src/AppState.hs"
  , "templates/project/src/AppView.hs"
  , "templates/project/src/Main.hs"
  , "templates/project/templates/readme.st"
  ]

-- does not work for folders
cp :: (FilePath, FilePath) -> IO ()
cp (src, dest) = do
  exists <- doesFileExist dest
  if exists
    then putStrLn $ "File exists already, skipping: " ++ dest
    else copyFile src dest

