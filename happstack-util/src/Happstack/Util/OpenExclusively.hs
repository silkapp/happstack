{-# LANGUAGE CPP #-}
 -- | Cross platform way to open a file exclusively
module Happstack.Util.OpenExclusively
    ( -- | Cross platform way to open a file exclusively
      openExclusively
    ) where
    
import System.IO

#ifdef mingw32_HOST_OS
-- Windows opens files for exclusive writing by default
openExclusively :: FilePath -> IO Handle
openExclusively fp = openFile fp ReadWriteMode
#endif

#ifndef mingw32_HOST_OS
import System.Posix.IO

-- Unix needs to use a special open call to open files for exclusive writing
openExclusively :: FilePath -> IO Handle
openExclusively fp =
    fdToHandle =<< openFd fp ReadWrite (Just 0o600) flags
    where flags = defaultFileFlags {exclusive = True, trunc = True}
#endif

