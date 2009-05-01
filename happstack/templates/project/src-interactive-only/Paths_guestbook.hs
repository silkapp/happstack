module Paths_guestbook (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [1,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = error "Paths_guestbook: bindir undefined in interactive mode"
libdir     = error "Paths_guestbook: bindir undefined in interactive mode"
datadir    = "."
libexecdir = error "Paths_guestbook: libexecdir undefined in interactive mode"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "guestbook_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "guestbook_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "guestbook_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "guestbook_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
