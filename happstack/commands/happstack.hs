-- happstack command
module Main where
import Paths_happstack
  (getDataFileName)
import System.Environment (getArgs)
import System.Directory
  (getDirectoryContents
  ,getCurrentDirectory
  ,setCurrentDirectory
  ,createDirectoryIfMissing
  ,copyFile
  ,canonicalizePath)
import System.FilePath
  (takeFileName, combine)
import Control.Monad
  (mapM, mapM_)
import Control.Exception
  (finally)
  
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
processArgs args = do
  case args of
    ["new", "project", dir] -> NewProjectCmd dir
    ["help"]                -> HelpCmd
    _                       -> HelpCmd
    
newProject :: FilePath -> IO ()
newProject dir = do
  -- create folders           
  createDirectoryIfMissing True (combine dir "bin")
  createDirectoryIfMissing True (combine dir "dist")
  createDirectoryIfMissing True (combine dir "src")
  createDirectoryIfMissing True (combine (combine dir "public") "entries")

  -- FIXME: OMG THE REST OF THIS IS SO FRACKING HACKY!THIS IS SO HACKY.
  -- ... but it should work on unix / win32

  -- copy files over to current directory
  binpaths <- ls =<< getDataFileName "templates/project/bin"
  srcpaths <- ls =<< getDataFileName "templates/project/src"
  let from = binpaths ++ srcpaths
  let to   = map (combine (combine dir "bin") . takeFileName) binpaths ++
             map (combine (combine dir "src") . takeFileName) srcpaths
  mapM_ cp (zip from to)
  
  index_html <- (getDataFileName "templates/project/public/index.html")
  cp (index_html,
      (combine (combine dir "public") "index.html")) 
  new_html <- (getDataFileName "templates/project/public/entries/new.html")
  cp (new_html,
      (combine (combine (combine dir "public") "entries") "new.html")) 

-- does not work for folders
cp :: (FilePath, FilePath) -> IO ()
cp = uncurry copyFile

-- works only on dirs, list contents with canonical paths
ls :: FilePath -> IO [FilePath]
ls dir = do
  cwd <- getCurrentDirectory
  (do setCurrentDirectory dir
      paths <- getDirectoryContents "."
      let paths' = filter (not . (`elem` [".", ".."])) paths
      mapM canonicalizePath paths') `finally` setCurrentDirectory cwd

