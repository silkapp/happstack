{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Happstack.Util.FileManip (always, find) where

import qualified System.PosixCompat.Files as F
import Control.Monad.State
import qualified Control.Exception.Extensible as E
import System.IO
import Data.List (sort)
import System.Directory (getDirectoryContents)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.FilePath ((</>))


-- | Information collected during the traversal of a directory.
data FileInfo = FileInfo
    {
      infoPath :: FilePath -- ^ file path
    , infoDepth :: Int -- ^ current recursion depth
    , infoStatus :: F.FileStatus -- ^ status of file
    } deriving (Eq)
instance Eq F.FileStatus where
    a == b = F.deviceID a == F.deviceID b &&
             F.fileID a == F.fileID b

-- | Construct a 'FileInfo' value.

mkFI :: FilePath -> Int -> F.FileStatus -> FileInfo

mkFI = FileInfo

-- | Monadic container for file information, allowing for clean
-- construction of combinators.  Wraps the 'State' monad, but doesn't
-- allow 'get' or 'put'.
newtype FindClause a = FC { runFC :: State FileInfo a }
    deriving (Functor, Monad)

-- | Run the given 'FindClause' on the given 'FileInfo' and return its
-- result.  This can be useful if you are writing a function to pass
-- to 'fold'.
--
-- Example:
--
-- > myFoldFunc :: a -> FileInfo -> a
-- > myFoldFunc a i = let useThisFile = evalClause (fileName ==? "foo") i
-- >                  in if useThisFile
-- >                     then fiddleWith a
-- >                     else a
--
evalClause :: FindClause a -> FileInfo -> a
evalClause = evalState . runFC

evalFI :: FindClause a
       -> FilePath
       -> Int
       -> F.FileStatus
       -> a
evalFI m p d s = evalClause m (mkFI p d s)

type FilterPredicate = FindClause Bool
type RecursionPredicate = FindClause Bool

-- | List the files in the given directory, sorted, and without \".\"
-- or \"..\".
getDirContents :: FilePath -> IO [FilePath]

getDirContents dir = (sort . filter goodName) `liftM` getDirectoryContents dir
    where goodName "." = False
          goodName ".." = False
          goodName _ = True

-- | Search a directory recursively, with recursion controlled by a
-- 'RecursionPredicate'.  Lazily return a sorted list of all files
-- matching the given 'FilterPredicate'.  Any errors that occur are
-- dealt with by the given handler.
findWithHandler ::
    (FilePath -> E.SomeException -> IO [FilePath]) -- ^ error handler
    -> RecursionPredicate -- ^ control recursion into subdirectories
    -> FilterPredicate -- ^ decide whether a file appears in the result
    -> FilePath -- ^ directory to start searching
    -> IO [FilePath] -- ^ files that matched the 'FilterPredicate'

findWithHandler errHandler recurse filter path =
    E.handle (errHandler path) $ F.getSymbolicLinkStatus path >>= visit path 0
  where visit path depth st =
            if F.isDirectory st && evalFI recurse path depth st
              then unsafeInterleaveIO (traverse path (succ depth) st)
              else filterPath path depth st []
        traverse dir depth dirSt = do
            names <- E.catch (getDirContents dir) (errHandler dir)
            filteredPaths <- forM names $ \name -> do
                let path = dir </> name
                unsafeInterleaveIO $ E.handle (errHandler path)
                    (F.getSymbolicLinkStatus path >>= visit path depth)
            filterPath dir depth dirSt (concat filteredPaths)
        filterPath path depth st result =
            return $ if evalFI filter path depth st
                then path:result
                else result

-- | Search a directory recursively, with recursion controlled by a
-- 'RecursionPredicate'.  Lazily return a sorted list of all files
-- matching the given 'FilterPredicate'.  Any errors that occur are
-- ignored, with warnings printed to 'stderr'.
find :: RecursionPredicate -- ^ control recursion into subdirectories
     -> FilterPredicate -- ^ decide whether a file appears in the result
     -> FilePath -- ^ directory to start searching
     -> IO [FilePath] -- ^ files that matched the 'FilterPredicate'

find = findWithHandler warnOnError
    where warnOnError path err =
              hPutStrLn stderr (path ++ ": " ++ show err) >> return []

-- | Unconditionally return 'True'.
always :: FindClause Bool
always = return True

