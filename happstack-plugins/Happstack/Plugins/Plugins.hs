{-# LANGUAGE EmptyDataDecls, TemplateHaskell #-}
module Happstack.Plugins.Plugins
    ( rebuild
    , func
    , funcTH
    , withIO
    , PluginHandle(..)
    ) where

import Control.Applicative        ((<$>))
import Control.Concurrent.MVar    (MVar,readMVar,modifyMVar,modifyMVar_)
import Data.List                  (nub)
import Data.Maybe                 (mapMaybe)
import qualified Data.Map         as Map
import           Data.Map         (Map)
import Language.Haskell.TH.Syntax (Name(Name),NameFlavour(NameG), occString, modString)
import System.FilePath            (addExtension, dropExtension)
import System.Plugins.Load        (Module, Symbol, LoadStatus(..), getImports, load, unloadAll)
import System.Plugins.Make        (Errors, MakeStatus(..), MakeCode(..), makeAll)
import System.INotify             (INotify, WatchDescriptor, Event(..), EventVariety(..), addWatch, removeWatch)
import System.FilePath            (splitFileName)
import Unsafe.Coerce              (unsafeCoerce)

-- A very unsafe version of Data.Dynamic

data Sym

toSym :: a -> Sym
toSym = unsafeCoerce

fromSym :: Sym -> a
fromSym = unsafeCoerce

-- PluginHandle (iNotify, map of watched files)
--  The map of watched files contains:
--   ( WatchDescriptors of the file and its dependencies
--   , dependencies of the file 
--   , errors when compiling the file if any
--   , map of symbols defined in the file - this map contains:
--        ( a function which reloads the symbol
--        , the state of the symbol (probably the last call to the function in the first component)
--        )
--  )
newtype PluginHandle = PluginHandle (INotify, MVar (Map FilePath ([WatchDescriptor], [FilePath], Maybe Errors, Map Symbol (FilePath -> IO (Either Errors (Module, Sym)), Either Errors (Module, Sym)))))


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

rebuild :: PluginHandle   -- ^ list of currently loaded modules/symbols
        -> FilePath -- ^ source file to compile
        -> Bool
        -> IO ()
rebuild p@(PluginHandle (inotify, objMap)) fp forceReload =
    do putStrLn ("Rebuilding " ++ fp)
       makeStatus <- makeAll fp [] -- FIXME: allow user to specify additional flags, such as -O2
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
                         mapM_ (removeWatch inotify) oldWds
                         res <- mapM (load' objFilePath) (Map.assocs symbols)
                         imports <- map (\bn -> addExtension bn ".hs") <$> getImports (dropExtension objFilePath)
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


observeFiles :: PluginHandle -> FilePath -> [FilePath] -> IO [WatchDescriptor]
observeFiles p@(PluginHandle (inotify,_objMap)) fp imports = 
        mapM (\depFp -> do putStrLn ("Adding watch for: " ++ depFp)
                           let (d,f) = splitFileName depFp
                           addWatch inotify [Modify, Move, Delete] d $ \e ->
                                                do putStrLn ("Got event for " ++ depFp ++ ": " ++ show e)
                                                   case e of
                                                     Ignored -> return ()
                                                     Deleted { filePath = f' } | f==f' -> rebuild p fp False
                                                     MovedIn { filePath = f' } | f==f' -> rebuild p fp False
                                                     Modified { maybeFilePath = Just f' } | f==f' -> rebuild p fp False
                                                     _ -> return ()
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


