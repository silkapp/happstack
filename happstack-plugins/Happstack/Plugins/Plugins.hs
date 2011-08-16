{-# LANGUAGE EmptyDataDecls, TemplateHaskell #-}
module Happstack.Plugins.Plugins
    ( rebuild
    , func
    , funcTH
    , withIO
    , PluginHandle(..)
    , initPersistentINotify
    ) where

import Control.Applicative        ((<$>))
import Control.Concurrent.MVar    (MVar,readMVar,modifyMVar,modifyMVar_,newMVar)
import Data.List                  (nub)
import Data.Maybe                 (mapMaybe)
import qualified Data.Map         as Map
import           Data.Map         (Map)
import Language.Haskell.TH.Syntax (Name(Name),NameFlavour(NameG), occString, modString)
import System.FilePath            (addExtension, dropExtension)
import System.Plugins.Load        (Module, Symbol, LoadStatus(..), getImports, load, unloadAll)
import System.Plugins.Make        (Errors, MakeStatus(..), MakeCode(..), makeAll)
import System.INotify             (INotify, WatchDescriptor, Event(..), EventVariety(..), addWatch, removeWatch, initINotify)
import System.FilePath            (splitFileName)
import Unsafe.Coerce              (unsafeCoerce)

-- A very unsafe version of Data.Dynamic

data Sym

toSym :: a -> Sym
toSym = unsafeCoerce

fromSym :: Sym -> a
fromSym = unsafeCoerce

newtype PluginHandle = PluginHandle 
   ( PersistentINotify                              -- Inotify handle
   , MVar
       ( Map FilePath                               -- source file being observed
             ( [WatchDescriptorP]                    -- watch descriptor of the source file and its dependecies
             , [FilePath]                           -- depedencies of the source file
             , Maybe Errors                         -- errors when compiling the file if any
             , Map Symbol                           -- symbol defined in the source file
                   (FilePath -> IO (Either Errors (Module, Sym)) -- function for reloading the symbol
                   , Either Errors (Module, Sym))   -- the state of the symbol (probably the result of the last call to the function in the first component)
             )
       )
   )


funcTH :: PluginHandle -> Name -> IO (Either Errors a)
funcTH objMap name = 
    do let (fp, sym) = nameToFileSym name
       func objMap fp sym


withIO :: PluginHandle -> Name -> (a -> IO ()) -> IO ()
withIO objMap name use =
    do r <- funcTH objMap name
       case r of
         (Left e) -> putStrLn $ unlines e
         (Right f) -> use f

nameToFileSym :: Name -> (FilePath, Symbol)
nameToFileSym (Name occName (NameG _ _ mn)) =
    let dotToSlash '.' = '/'
        dotToSlash c   = c
        fp  = (map dotToSlash (modString mn)) ++ ".hs"
        sym = occString occName
    in (fp, sym)
nameToFileSym n = error $ "nameToFileSym failed because Name was not the right kind. " ++ show n

func :: PluginHandle -> FilePath -> Symbol -> IO (Either Errors a)
func ph@(PluginHandle (_inotify, objMap)) fp sym =
    do om <- readMVar objMap
       case Map.lookup fp om of
         Nothing -> 
             do addSymbol ph fp sym
                rebuild ph fp True
                func ph fp sym
         (Just (_, _, Just errs, _)) -> return $ Left errs
         (Just (_, _, Nothing, symbols)) ->
             case Map.lookup sym symbols of
               Nothing ->
                   do addSymbol ph fp sym
                      rebuild ph fp True
                      func ph fp sym
               (Just (_, Left errs)) -> return $ Left errs
               (Just (_, Right (_, dynSym))) -> return (Right $ fromSym dynSym)

replaceSuffix :: FilePath -> String -> FilePath
replaceSuffix p sfx = case [ i | (i,'.') <- zip [0..] p ] of
                        [] -> p++sfx
                        ixs -> take (last ixs) p ++ '.':sfx

rebuild :: PluginHandle   -- ^ list of currently loaded modules/symbols
        -> FilePath -- ^ source file to compile
        -> Bool
        -> IO ()
rebuild p@(PluginHandle (_inotify, objMap)) fp forceReload =
    do putStrLn ("Rebuilding " ++ fp)
       makeStatus <- makeAll fp ["-odir",".","-hidir",".","-o",replaceSuffix fp "o"] -- FIXME: allow user to specify additional flags, such as -O2
       case makeStatus of
         (MakeFailure errs) ->
             do unload <- modifyMVar objMap $ \om ->
                           case Map.lookup fp om of
                             Nothing -> do wds <- observeFiles p fp []
                                           return (Map.insert fp (wds, [], Just errs, Map.empty) om, [])
                             (Just (wds, deps, _, symbols)) ->
                                 let symbols' = Map.map (\(loader,_) -> (loader, Left errs)) symbols -- propogate error to all symbols
                                 in return (Map.insert fp (wds, deps, Just errs, symbols') om, unloadList symbols)
                mapM_ unloadAll unload 
                putStrLn $ unlines errs
         (MakeSuccess NotReq _objFilePath) | not forceReload -> 
                                               do putStrLn "skipped reload."
                                                  return ()
         (MakeSuccess _makeCode objFilePath) -> 
             do om <- readMVar objMap
                case Map.lookup fp om of
                  Nothing -> return ()
                  (Just (oldWds, _, _, symbols)) ->
                      do mapM_ unloadAll (unloadList symbols)
                         mapM_ removeWatchP oldWds
                         res <- mapM (load' objFilePath) (Map.assocs symbols)
                         imports <- map (\bn -> addExtension (mnameToPath bn) ".hs") <$> getImports (dropExtension objFilePath)
                         wds <- observeFiles p fp imports
                         modifyMVar_ objMap $ return . Map.insert fp (wds, [], Nothing, Map.fromList res)
    where
      unloadList symbols =
          nub $ mapMaybe (\(_, eSym) ->
                              case eSym of
                                (Left _)      -> Nothing
                                (Right (m,_)) -> Just m) (Map.elems symbols)

      load' :: FilePath 
            -> (Symbol, (FilePath -> IO (Either Errors (Module, Sym)), Either Errors (Module, Sym)))
            -> IO (Symbol, (FilePath -> IO (Either Errors (Module, Sym)), Either Errors (Module, Sym)))
      load' obj (symbol, (reloader, _)) =
          do r <- reloader obj
             case r of
               (Left errs) -> putStrLn $ unlines errs
               (Right _) -> return ()
             return (symbol, (reloader, r))

mnameToPath :: FilePath -> FilePath
mnameToPath = replace '.' '/' 
 where replace x y = foldr (\a r -> if x==a then y:r else a:r) []

observeFiles :: PluginHandle -> FilePath -> [FilePath] -> IO [WatchDescriptorP]
observeFiles p@(PluginHandle (inotify,_objMap)) fp imports = 
        mapM (\depFp -> do putStrLn ("Adding watch for: " ++ depFp)
                           let handler e = putStrLn ("Got event for " ++ depFp ++ ": " ++ show e) >> rebuild p fp False
                           addWatchP inotify depFp handler
             ) (fp:imports)
                                   

addSymbol :: PluginHandle -> FilePath -> Symbol -> IO ()
addSymbol p@(PluginHandle (_inotify, objMap)) sourceFP sym =
    do let reloader obj = 
               do putStrLn $ "loading " ++ sym ++ " from " ++ sourceFP
                  ldStatus <- load obj ["."] [] sym
                  case ldStatus of
                    (LoadSuccess m s) -> 
                        do putStrLn "Succeed." 
                           return (Right (m, toSym s))
                    (LoadFailure errs) -> 
                        do putStrLn "Failed."
                           return (Left errs)
           symVal       = (reloader, Left ["Not loaded yet.."])
       modifyMVar_ objMap $ \om ->
           case Map.lookup sourceFP om of
             Nothing -> do wds <- observeFiles p sourceFP []
                           return$ Map.insert sourceFP (wds, [], Nothing, Map.singleton sym symVal) om
             (Just (wds, deps, errs, symbols)) ->
                 let symbols' = Map.insert sym symVal symbols
                 in return$ Map.insert sourceFP (wds, deps, errs, symbols') om
                          
       return ()


-- Keeps watching a file even after it has been deleted and created again.
--
-- It does so by observing the folder which contains the file. When no files
-- are observed in a given folder, the folder stops being observed.
data PersistentINotify = PersistentINotify 
         INotify                       -- INotify handle
         (MVar 
           (Map FilePath                 -- Folder containing the file
                ( WatchDescriptor        -- Watch descriptor of the folder
                , Map String             -- File being observed
                      (Event -> IO ())   -- Handler to run on file events
                )
           )
         )

data WatchDescriptorP = WatchDescriptorP PersistentINotify FilePath

initPersistentINotify :: IO PersistentINotify
initPersistentINotify = do
  iN <- initINotify
  fmvar <- newMVar Map.empty
  return$ PersistentINotify iN fmvar

-- Replacement for splitFileName which returns "." instead of an empty folder.
splitFileName' :: FilePath -> (FilePath,String)
splitFileName' fp =
   let (d,f) = splitFileName fp
    in (if null d then "." else d,f)

addWatchP :: PersistentINotify -> FilePath -> (Event -> IO ()) -> IO WatchDescriptorP
addWatchP piN@(PersistentINotify iN fmvar) fp hdl = 
   let (d,f) = splitFileName' fp
    in modifyMVar fmvar$ \fm ->
   case Map.lookup d fm of
     Nothing -> do
         wd <- addWatch iN [Modify, Move, Delete] d $ \e -> do 
                  case e of
                     Ignored -> return ()
                     Deleted { filePath = f' } -> callHandler e d f'
                     MovedIn { filePath = f' } -> callHandler e d f'
                     Modified { maybeFilePath = Just f' } -> callHandler e d f'
                     _ -> return ()
         return ( Map.insert d (wd,Map.singleton f hdl) fm 
                , WatchDescriptorP piN fp 
                )
     Just (wd,ffm) -> return ( Map.insert d (wd,Map.insert f hdl ffm) fm
                             , WatchDescriptorP piN fp
                             )
  where
     callHandler e d f = do 
       fm <- readMVar fmvar 
       case Map.lookup d fm of 
         Nothing -> return ()
         Just (_,ffm) -> case Map.lookup f ffm of
                           Nothing -> return ()
                           Just mhdl -> mhdl e
 

removeWatchP :: WatchDescriptorP -> IO ()
removeWatchP (WatchDescriptorP (PersistentINotify iN fmvar) fp) =
   let (d,f) = splitFileName' fp
    in modifyMVar_ fmvar$ \fm ->
   case Map.lookup d fm of
     Nothing -> error$ "removeWatchP: invalid handle for file "++fp
     Just (wd,ffm) -> let ffm' = Map.delete f ffm
                       in if Map.null ffm' then removeWatch wd >> return (Map.delete d fm)
                            else return (Map.insert d (wd,ffm') fm)
  
   
