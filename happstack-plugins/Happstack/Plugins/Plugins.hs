{-# LANGUAGE EmptyDataDecls, TemplateHaskell #-}
module Happstack.Plugins.Plugins
    ( rebuild
    , func
    , funcTH
    , withIO
    , PluginHandle(..)
    ) where

import Control.Applicative        ((<$>))
import Data.IORef                 (IORef, atomicModifyIORef, readIORef)
import Data.List                  (nub)
import Data.Maybe                 (mapMaybe)
import qualified Data.Map         as Map
import           Data.Map         (Map)
import Language.Haskell.TH.Syntax (Name(Name),NameFlavour(NameG), occString, modString)
import System.FilePath            (addExtension, dropExtension)
import System.Plugins.Load        (Module, Symbol, LoadStatus(..), getImports, load, unloadAll)
import System.Plugins.Make        (Errors, MakeStatus(..), MakeCode(..), makeAll)
import System.INotify             (INotify, WatchDescriptor, Event(..), EventVariety(..), addWatch, removeWatch)
import Unsafe.Coerce              (unsafeCoerce)

-- A very unsafe version of Data.Dynamic

data Sym

toSym :: a -> Sym
toSym = unsafeCoerce

fromSym :: Sym -> a
fromSym = unsafeCoerce

newtype PluginHandle = PluginHandle (INotify, IORef (Map FilePath ([WatchDescriptor], [FilePath], Maybe Errors, Map Symbol (FilePath -> IO (Either Errors (Module, Sym)), Either Errors (Module, Sym)))))

atomicModifyIORef' :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef' ref fn = atomicModifyIORef ref (\val -> (fn val, ()))

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
    do om <- readIORef objMap
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
rebuild (PluginHandle (inotify, objMap)) fp forceReload =
    do putStrLn ("Rebuilding " ++ fp)
       makeStatus <- makeAll fp [] -- FIXME: allow user to specify additional flags, such as -O2
       case makeStatus of
         (MakeFailure errs) ->
             do unload <- atomicModifyIORef objMap $ \om ->
                           case Map.lookup fp om of
                             Nothing -> (Map.insert fp ([], [], Just errs, Map.empty) om, [])
                             (Just (wds, deps, _, symbols)) ->
                                 let symbols' = Map.map (\(loader,_) -> (loader, Left errs)) symbols -- propogate error to all symbols
                                 in (Map.insert fp (wds, deps, Just errs, symbols') om, unloadList symbols)
                mapM_ unloadAll unload 
                putStrLn $ unlines errs
         (MakeSuccess NotReq _objFilePath) | not forceReload -> 
                                               do putStrLn "skipped reload."
                                                  return ()
         (MakeSuccess _makeCode objFilePath) -> 
             do om <- readIORef objMap
                case Map.lookup fp om of
                  Nothing -> return ()
                  (Just (oldWds, _, _, symbols)) ->
                      do mapM_ unloadAll (unloadList symbols)
                         mapM_ (removeWatch inotify) oldWds
                         res <- mapM (load' objFilePath) (Map.assocs symbols)
                         imports <- map (\bn -> addExtension bn ".hs") <$> getImports (dropExtension objFilePath)
                         wds <- mapM (\depFp -> putStrLn ("Adding watch for: " ++ depFp) >> addWatch inotify [Modify, Move, Delete] depFp 
                                                (\e -> do putStrLn ("Got event for " ++ depFp ++ ": " ++ show e)
                                                          case e of
                                                            Ignored -> return ()
                                                            _ -> rebuild (PluginHandle (inotify, objMap)) fp False)) (fp:imports)
                         atomicModifyIORef' objMap $ Map.insert fp (wds, [], Nothing, Map.fromList res)
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

addSymbol :: PluginHandle -> FilePath -> Symbol -> IO ()
addSymbol (PluginHandle (_inotify, objMap)) sourceFP sym =
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
       atomicModifyIORef' objMap $ \om ->
           case Map.lookup sourceFP om of
             Nothing -> Map.insert sourceFP ([], [], Nothing, Map.singleton sym symVal) om
             (Just (wds, deps, errs, symbols)) ->
                 let symbols' = Map.insert sym symVal symbols
                 in Map.insert sourceFP (wds, deps, errs, symbols') om
                          
       return ()


