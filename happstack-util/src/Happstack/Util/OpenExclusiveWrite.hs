 -- | Cross platform way to open a file exclusively for writing
module Happstack.Util.OpenExclusiveWrite
    ( -- | Cross platform way to open a file exclusively for writing
      openExclusiveWrite
    ) where
    
import System.IO

#ifdef mingw32_HOST_OS
-- Windows opens files for exclusive writing by default
openExclusiveWrite :: FilePath -> IO Handle
openExclusiveWrite fp = openFile fp WriteMode
#endif

#ifndef mingw32_HOST_OS
import System.Posix.IO

-- Unix needs to use a special open call to open files for exclusive writing
openExclusiveWrite :: FilePath -> IO Handle
openExclusiveWrite fp =
    fdToHandle =<< openFd fp WriteOnly (Just 0o600) flags
    where flags = defaultFileFlags {exclusive = True, trunc = True}
#endif

