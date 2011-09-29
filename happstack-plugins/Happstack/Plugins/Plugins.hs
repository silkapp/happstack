{-# LANGUAGE EmptyDataDecls, TemplateHaskell #-}
module Happstack.Plugins.Plugins
    ( rebuild
    , func
    , funcTH
    , withIO
    , funcTH'
    , withIO'
    , PluginHandle
    , initPlugins
    ) where

import Control.Applicative        ((<$>))
import Control.Concurrent.MVar    (MVar,readMVar,modifyMVar_,newMVar,modifyMVar)
import Control.Concurrent         (threadDelay,killThread,ThreadId,forkIO)
import Control.Exception          (bracketOnError,bracket)
import Control.Monad(when)
import Data.List                  (nub)
import Data.Maybe                 (mapMaybe)
import qualified Data.Map         as Map
import           Data.Map         (Map)
import Language.Haskell.TH.Syntax (Name(Name),NameFlavour(NameG), occString, modString)
import System.FilePath            (addExtension, dropExtension)
import System.Plugins.Load        (Module, Symbol, LoadStatus(..), getImports, load, unloadAll)
import System.Plugins.Make        (Errors, MakeStatus(..), MakeCode(..), makeAll)
import Unsafe.Coerce              (unsafeCoerce)

import Happstack.Plugins.FileSystemWatcher

-- A very unsafe version of Data.Dynamic

data Sym

toSym :: a -> Sym
toSym = unsafeCoerce

fromSym :: Sym -> a
fromSym = unsafeCoerce

data RecompilationState = RecompilationState 
      { rsRecompiling :: MVar Bool                     -- Is recompilation in progress?
      , rsRecompilationNeeded :: MVar Bool             -- Is recompilation needed?
      , rsTimer :: Timer
      }

data PluginHandle = PluginHandle 
  { phWatcher :: FSWatcher                         -- Inotify handle
  , phObjMap :: MVar
     ( Map FilePath                                -- source file being observed
         ( [FSWatchDescriptor]                       -- watch descriptor of the source file and its dependecies
         , [FilePath]                                -- depedencies of the source file
         , Maybe Errors                              -- errors when compiling the file if any
         , Map Symbol                                -- symbol defined in the source file
             ( FilePath 
                 -> IO (Either Errors (Module, Sym))   -- function for reloading the symbol
             , Errors                                  -- error from last recompilation attempt if any
             , Maybe (Module, Sym)                     -- last succesfully loaded symbol
             )
         , RecompilationState
         )
     )
  }


-- |initialize the plugin system and return a 'PluginHandle'
initPlugins :: IO PluginHandle
initPlugins =
    do inotify <- initFSWatcher
       objMap <- newMVar Map.empty
       return$ PluginHandle inotify objMap

newRecompilationState :: IO RecompilationState
newRecompilationState =  do
    recompiling <- newMVar False
    recompilationNeeded <- newMVar False
    timer <- newTimer 
    return RecompilationState 
             { rsRecompiling = recompiling
             , rsRecompilationNeeded = recompilationNeeded
             , rsTimer = timer
             }
 

funcTH :: PluginHandle -> Name -> IO (Either Errors a)
funcTH objMap name = fmap reformatOutput$ funcTH' objMap name []

funcTH' :: PluginHandle -> Name -> [String] -> IO (Errors,Maybe a)
funcTH' objMap name args = 
    do let (fp, sym) = nameToFileSym name
       func' objMap fp sym args

reformatOutput :: (Errors,Maybe a) -> Either Errors a
reformatOutput ([],Just a) = Right a
reformatOutput ([],_) = Left ["Module not loaded."]
reformatOutput (errs,_) = Left errs



withIO :: PluginHandle -> Name -> (a -> IO ()) -> IO ()
withIO objMap name use = withIO' objMap name use []

withIO' :: PluginHandle -> Name -> (a -> IO ()) -> [String] -> IO ()
withIO' objMap name use args =
    do (errs,ma) <- funcTH' objMap name args
       when (not$ null errs)$ putStrLn $ unlines errs
       maybe (return ()) use ma



nameToFileSym :: Name -> (FilePath, Symbol)
nameToFileSym (Name occName (NameG _ _ mn)) =
    let dotToSlash '.' = '/'
        dotToSlash c   = c
        fp  = (map dotToSlash (modString mn)) ++ ".hs"
        sym = occString occName
    in (fp, sym)
nameToFileSym n = error $ "nameToFileSym failed because Name was not the right kind. " ++ show n

func :: PluginHandle -> FilePath -> Symbol -> IO (Either Errors a)
func ph fp sym = fmap reformatOutput$ func' ph fp sym []

func' :: PluginHandle -> FilePath -> Symbol -> [String] -> IO (Errors,Maybe a)
func' ph fp sym args =
    do om <- readMVar$ phObjMap ph
       case Map.lookup fp om of
         Nothing -> 
             do bracketOnError
                  (addSymbol ph fp sym args)
                  (const$ deleteSymbol ph fp sym)
                  (const$ rebuild' ph fp True args)
                func' ph fp sym args
         (Just (_, _, merrs, symbols, _)) ->
             case Map.lookup sym symbols of
               Nothing ->
                 case merrs of
                   Nothing -> 
                     do bracketOnError
                          (addSymbol ph fp sym args)
                          (const$ deleteSymbol ph fp sym)
                          (const$ rebuild' ph fp True args)
                        func' ph fp sym args
                   Just errs -> 
                     return (errs,Nothing)
               Just (_, [], mm) -> 
                    return (maybe [] id merrs, fmap (fromSym . snd) mm)
               Just (_, errs, mm) -> 
                    return (errs,fmap (fromSym . snd) mm)

replaceSuffix :: FilePath -> String -> FilePath
replaceSuffix p sfx = case [ i | (i,'.') <- zip [0..] p ] of
                        [] -> p++sfx
                        ixs -> take (last ixs) p ++ '.':sfx

rebuild :: PluginHandle   -- ^ list of currently loaded modules/symbols
        -> FilePath -- ^ source file to compile
        -> Bool
        -> IO ()
rebuild p fp forceReload = rebuild' p fp forceReload []
 
rebuild' :: PluginHandle -> FilePath -> Bool -> [String] -> IO ()
rebuild' p fp forceReload args =
    do rs <- readMVar (phObjMap p) >>= maybe newRecompilationState (\(_,_,_,_,rs)->return rs) . Map.lookup fp
       bracket
         (signalRecompilationStarted rs)
         (\compile -> when compile$ signalRecompilationFinished rs (rebuild'' rs))
         (\compile -> when compile$ rebuild'' rs)
  where
    rebuild'' :: RecompilationState -> IO ()
    rebuild'' rs = do
       putStrLn ("Rebuilding " ++ fp)
       makeStatus <- makeAll fp (["-odir",".","-hidir",".","-o",replaceSuffix fp "o"]++args)
       case makeStatus of
         (MakeFailure errs) ->
             do modifyMVar_ (phObjMap p) $ \om ->
                           case Map.lookup fp om of
                             Nothing -> do wds <- observeFiles p fp [] args rs
                                           return$ Map.insert fp (wds, [], Just errs, Map.empty, rs) om
                             Just (wds, deps, _, symbols,_) ->
                                 let symbols' = Map.map (\(loader,_,mm) -> (loader,errs,mm)) symbols -- propogate error to all symbols
                                 in return$ Map.insert fp (wds, deps, Just errs, symbols',rs) om
                putStrLn $ unlines errs
         (MakeSuccess NotReq _objFilePath) | not forceReload -> 
                                               do putStrLn "skipped reload."
                                                  return ()
         (MakeSuccess _makeCode objFilePath) -> 
             do om <- readMVar$ phObjMap p
                case Map.lookup fp om of
                  Nothing -> return ()
                  Just (oldWds, _, _, symbols,_) ->
                      do mapM_ unloadAll (unloadList symbols)
                         mapM_ removeWatch oldWds
                         res <- mapM (load' objFilePath) (Map.assocs symbols)
                         imports <- map (\bn -> addExtension (mnameToPath bn) ".hs") <$> getImports (dropExtension objFilePath)
                         wds <- observeFiles p fp imports args rs
                         modifyMVar_ (phObjMap p) $ return . Map.insert fp (wds, [], Nothing, Map.fromList res,rs)
   
    unloadList symbols =
          nub$ mapMaybe (\(_, _, mm) -> fmap fst mm)$ Map.elems symbols

    load' :: FilePath 
            -> (Symbol, (FilePath -> IO (Either Errors (Module, Sym)), Errors, Maybe (Module, Sym)))
            -> IO (Symbol, (FilePath -> IO (Either Errors (Module, Sym)), Errors, Maybe (Module, Sym)))
    load' obj (symbol, (reloader, _, _mm)) =
          do r <- reloader obj
             case r of
               (Left errs) -> putStrLn $ unlines errs
               (Right _) -> return ()
             return (symbol, (reloader, either id (const []) r,either (const Nothing) Just r))

mnameToPath :: FilePath -> FilePath
mnameToPath = replace '.' '/' 
 where replace x y = foldr (\a r -> if x==a then y:r else a:r) []

observeFiles :: PluginHandle -> FilePath -> [FilePath] -> [String] -> RecompilationState -> IO [FSWatchDescriptor]
observeFiles p fp imports args rs = 
        mapM (\depFp -> do putStrLn ("Adding watch for: " ++ depFp)
                           let handler = putStrLn ("Got event for " ++ depFp) >> signalRecompilationNeeded rs (rebuild' p fp False args)
                           addWatch (phWatcher p) depFp handler
             ) (fp:imports)
                                   

addSymbol :: PluginHandle -> FilePath -> Symbol -> [String] -> IO ()
addSymbol p sourceFP sym args =
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
           symVal       = (reloader, [], Nothing)
       modifyMVar_ (phObjMap p) $ \om ->
           case Map.lookup sourceFP om of
             Nothing -> do rs <- newRecompilationState
                           wds <- observeFiles p sourceFP [] args rs
                           return$ Map.insert sourceFP (wds, [], Nothing, Map.singleton sym symVal,rs) om
             (Just (wds, deps, errs, symbols,rs)) ->
                 let symbols' = Map.insert sym symVal symbols
                 in return$ Map.insert sourceFP (wds, deps, errs, symbols',rs) om
                          
       return ()

deleteSymbol :: PluginHandle -> FilePath -> Symbol -> IO ()
deleteSymbol ph sourceFP sym =
       modifyMVar_ (phObjMap ph) $ \om ->
           case Map.lookup sourceFP om of
             Nothing -> return om
             Just (wds, deps, errs, symbols,rs) ->
                 let symbols' = Map.delete sym symbols
                  in return$ Map.insert sourceFP (wds, deps, errs, symbols',rs) om

--------------------------------
-- Recompilation handling
--------------------------------

-- | Indicates that recompilation is needed. If compilation is in progress
-- recompilation will be done afterwards, otherwise recompilation starts
-- in a few milliseconds after the call.
signalRecompilationNeeded :: RecompilationState -- ^ Internal state for synchronizing recompilation requests
                          -> IO ()              -- ^ Command which starts recompilation
						  -> IO ()
signalRecompilationNeeded rs recomp = do
    modifyMVar_ (rsRecompilationNeeded rs)$ const$ return True
    mvarIf (rsRecompiling rs) (return ()) (testRecompilation rs recomp)


-- | Indicates that recompilation is starting. The returned boolean indicates whether
-- there is any other attempt for recompilation in progress.
signalRecompilationStarted :: RecompilationState -> IO Bool
signalRecompilationStarted rs = do
     modifyMVar (rsRecompiling rs)$ \b -> do
       when (not b)$ modifyMVar_ (rsRecompilationNeeded rs)$ const$ return False
       return (True,not b)


-- | Indicates that recompilation has stopped.
-- It test whether recompilation is needed, and if needed it starts it again.
signalRecompilationFinished :: RecompilationState   -- ^ Internal state for synchronizing recompilation requests
                            -> IO ()                -- ^ Command which starts recompilation
							-> IO ()
signalRecompilationFinished rs recomp = do 
     modifyMVar_ (rsRecompiling rs)$ const$ return False
     testRecompilation rs recomp

-- | Fires the recompilation command after a small timeout if recompilation is needed
-- after the timeout expired. The timeout allows to delay recompilation if new requests
-- for recompilation arrive before the recompilation starts.
testRecompilation :: RecompilationState -> IO () -> IO ()
testRecompilation rs recomp = setTimer (rsTimer rs) (400*1000)$
                  mvarIf (rsRecompilationNeeded rs) recomp (return ())

mvarIf :: MVar Bool -> IO a -> IO a -> IO a
mvarIf mb th els = readMVar mb >>= \b -> if b then th else els

-------------------------------- 
-- Timer data type
--------------------------------

newtype Timer = Timer (MVar (Maybe ThreadId))

newTimer :: IO Timer
newTimer = fmap Timer$ newMVar Nothing

-- Register an action to perform after a timeout.
-- Cancels previous timer setting.
setTimer :: Timer -> Int -> IO () -> IO ()
setTimer (Timer mmtid) delay action = modifyMVar_ mmtid$ \mtid -> do
          maybe (return ()) killThread mtid
          fmap Just$ forkIO$ threadDelay delay >> modifyMVar_ mmtid (const (return Nothing)) >> action

