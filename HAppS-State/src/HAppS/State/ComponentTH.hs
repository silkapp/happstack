{-# LANGUAGE FlexibleInstances, TemplateHaskell, CPP, PatternGuards #-}
module HAppS.State.ComponentTH
    ( mkMethods
    ) where

import Data.Char
import Language.Haskell.TH

import HAppS.State.Types
import HAppS.Data.Serialize
import HAppS.State.ComponentSystem

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Maybe

import Data.List

import Data.Generics.Basics

nubCxt tsQ
    = do ts <- cxt tsQ
         return $ nub ts

{-
  Error cases:
    not all state keys are tyvars.
    method not using the keys in either the args or the result.
    Checked: component not being a data declaration with a single constructor.
    Checked: method using a tyvar that isn't a key.
    Checked: method is not a function.
-}
mkMethods :: Name -> [Name] -> Q [Dec]
mkMethods componentName componentMethods
    = do keys <- liftM (requireSimpleCon componentName) $ reify componentName
         methodInfos <- getMethodInfos keys componentMethods
         ds1 <- genEventInstances methodInfos
         let handlers = genComponentHandlers methodInfos
             stType = mkType componentName keys
             context' = mkKeyConstraints keys ++
                        concatMap (mkMethodConstraints keys) methodInfos
         ds2 <- instanceD (nubCxt context') (appT (conT ''Methods) stType) [ funD 'methods [clause [wildP] (normalB handlers) []] ]
         ds4 <- genMethodStructs [''Typeable] methodInfos
         ds5 <- genSerializeInstances methodInfos
         return (ds1 ++ [ds2] ++ ds4 ++  ds5 )

mkKeyConstraints :: [Name] -> [TypeQ]
mkKeyConstraints keys
    = [ appT (conT ''Typeable) (varT key) | key <- keys ] ++
      [ appT (conT ''Serialize) (varT key) | key <- keys ]

mkMethodConstraints :: [Name] -> MethodInfo -> [TypeQ]
mkMethodConstraints keys method
    = map return (substMethodContext method keys)

substMethodContext method keys
    = let relation = zip (methodKeys method) keys
          worker (VarT old) | Just new <- lookup old relation
                   = VarT new
          worker (AppT l r) = AppT (worker l) (worker r)
          worker (ForallT cxt names t) = ForallT cxt names (worker t)
          worker t = t
      in map worker (methodContext method)


mkType name args = foldl appT (conT name) (map varT args)

genSerializeInstances :: [MethodInfo] -> Q [Dec]
genSerializeInstances methods
    = liftM concat $ forM methods $ \method ->
      let constraints = nubCxt $ mkKeyConstraints (methodKeys method) ++ map return (methodContext method)
          upperMethod = upperName (methodName method)
          encode = do args <- replicateM (length (methodArgs method)) (newName "arg")
                      lamE [conP upperMethod $ map varP args ] $
                           doE $ [ noBindS $ appE (varE 'safePut) (varE arg) | arg <- args] ++
                                 [ noBindS [| return () |] ]
          decode = do args <- replicateM (length (methodArgs method)) (newName "arg")
                      doE $ [ bindS (varP arg) (varE 'safeGet) | arg <- args] ++
                            [ noBindS $ appE (varE 'return) $ foldl appE (conE upperMethod) $ map varE args ]
      in do s <- instanceD constraints
                   (appT (conT ''Serialize) (mkType (upperName (methodName method)) (methodKeys method)))
                   [funD 'putCopy [clause [] (normalB [| contain . $(encode) |]) []]
                   ,funD 'getCopy [clause [] (normalB [| contain $(decode) |]) []]]
            v <- instanceD constraints (appT (conT ''Version) (mkType (upperName (methodName method)) (methodKeys method))) []
            return [s,v]


{-
  [ Update $ \(SetComponent c) -> setComponent c
  , Query $ \GetComponent -> getComponent ]
-}
genComponentHandlers :: [MethodInfo] -> ExpQ
genComponentHandlers methods
    = do let localHandlers = flip map methods $ \method ->
                        let upName = upperName (methodName method)
                        in do args <- replicateM (length (methodArgs method)) (newName "arg")
                              appE (conE (methodEv method)) $
                                lamE [conP upName (map varP args)] $ foldl appE (varE (methodName method)) $ map varE args
             handlers = listE localHandlers
         handlers


genEventInstances :: [MethodInfo] -> Q [Dec]
genEventInstances methodsInfo
    = mapM genEventInstance methodsInfo

genEventInstance :: MethodInfo -> Q Dec
genEventInstance method
    = do let keys = methodKeys method
             eventType = foldl appT (conT (upperName (methodName method))) (map varT keys)
         instanceD (nubCxt $ [appT (conT ''Serialize) eventType
                             ,appT (conT ''Serialize) (return (methodResult method))]
                          ++ mkKeyConstraints keys
                          ++ mkMethodConstraints keys method
                   )
                   (appT (appT (conT (methodClass method)) eventType) (return (methodResult method)))
                   []


genMethodStructs :: [Name] -> [MethodInfo] -> Q [Dec]
genMethodStructs derv methods
    = liftM concat (mapM (genMethodStruct derv) methods)

-- FIXME: allow class constraints on keys.
genMethodStruct :: [Name] -> MethodInfo -> Q [Dec]
genMethodStruct derv method
    = do let c = NormalC (upperName (methodName method)) (zip (repeat NotStrict ) (methodArgs method))
         return [ DataD [] (upperName (methodName method)) (methodKeys method) [c] (derv) ]

upperName = mkName . upperFirst . nameBase

upperFirst :: String -> String
upperFirst (x:xs) = toUpper x : xs
upperFirst "" = error "ComponentTH.UpperFirst []"

data MethodInfo = Method { methodName   :: Name
                         , methodKeys   :: [Name]
                         , methodContext:: [Type]
                         , methodArgs   :: [Type]
                         , methodClass  :: Name
                         , methodEv     :: Name
                         , methodResult :: Type
                         }

-- get and validate method information.
getMethodInfos :: [Name] -> [Name] -> Q [MethodInfo]
getMethodInfos sessionKeys names
    = do ms <- mapM getMethodInfo names
         mapM worker ms
    where worker m | length (methodKeys m) /= length sessionKeys
                       = error $ "Inconsistent keys: " ++ pprint (methodName m) ++ ": " ++ show (sessionKeys, methodKeys m)
                   | otherwise = case compare (sort (methodTyVars m)) (sort (methodKeys m)) of
                                   EQ -> return m
                                   GT -> error $ "Method too general: " ++ pprint (methodName m)
                                   LT -> error $ "Method not general enough: " ++ pprint (methodName m)
          getArgKeys (AppT t1 t2) = getArgKeys t1 ++ getArgKeys t2
          getArgKeys (VarT key) = [key]
          getArgKeys _ = []
          methodTyVars m = nub $ concatMap getArgKeys (methodResult m:methodArgs m)

getMethodInfo :: Name -> Q MethodInfo
getMethodInfo method
    = do methodInfo <- reify method
         case methodInfo of
           VarI _name funcType _decl _fixity -> return (getTypes funcType){methodName = method}
           _ -> error $ "Method is not a function: " ++ nameBase method ++ " is a " ++ showInfo methodInfo


showInfo (ClassI _) = "class"
showInfo (TyConI _) = "type constructor"
showInfo (PrimTyConI _ _ _) = "primitive type constructor"
showInfo (DataConI _ _ _ _) = "data constructor"
showInfo (VarI _ _ _ _) = "variable"
showInfo (TyVarI _ _) = "type variable"
showInfo x = pprint x


-- Cases:
--  forall m. MonadState state m => X -> m Y
--  forall key. key -> Update ()
--  forall key m. MonadState state m => key -> m ()
--  X -> Ev (ReaderT state STM) Y
--  X -> Ev (StateT state STM) Y
getTypes :: Type -> MethodInfo
getTypes (ForallT _ cxt t) = getTypes' cxt t
getTypes t = getTypes' [] t

-- FIXME: only allow type variables used by the component.
getTypes' :: Cxt -> Type -> MethodInfo
getTypes' cxt t
    = case runWriter (worker t) of
        ((keys,className, typeName, res), args) -> Method { methodName = error "Method name not set", methodKeys = keys
                                                          , methodContext = filter (isRelevant keys) cxt
                                                          , methodArgs = args [], methodClass = className
                                                          , methodEv = typeName, methodResult = res}
    where -- recursive case: A -> B
          worker (AppT (AppT ArrowT t1) t2)
              = do tell (t1:)
                   worker t2
          -- end case: Update state res  ||  Query state res
          worker (AppT (AppT (ConT c) state) r)
              | c == ''Update = return (getStateKeys state,''UpdateEvent, 'Update, r)
              | c == ''Query  = return (getStateKeys state,''QueryEvent, 'Query, r)
          -- end case: Ev (ReaderT state STM) res  ||  Ev (StateT state STM) res
          worker (AppT (AppT (ConT _ev) (AppT (AppT (ConT m) state) (ConT _stm))) r)
              | m == ''StateT  = return (getStateKeys state,''UpdateEvent, 'Update, r)
              | m == ''ReaderT = return (getStateKeys state,''QueryEvent, 'Query, r)
          -- end case: m res    (check if m is an instance of MonadState)
          worker (AppT name r)
              | Just state <- isMonadState cxt name  = return (getStateKeys state, ''UpdateEvent, 'Update, r)
              | Just state <- isMonadReader cxt name = return (getStateKeys state, ''QueryEvent, 'Query, r)
          -- error case
          worker t = error ("Unexpected method type: " ++ pprint t)

getStateKeys (AppT r r') = getStateKeys r ++ getStateKeys r'
getStateKeys (VarT key) = [key]
getStateKeys (ConT _st) = []
getStateKeys v = error $ "Bad state type: " ++ pprint v ++ " (expected a constant, an application or a type variable)"

isMonadState cxt name = listToMaybe [ state | AppT (AppT (ConT m) state) mName <- cxt, mName == name, m == ''MonadState ]
isMonadReader cxt name = listToMaybe [ state | AppT (AppT (ConT m) state) mName <- cxt, mName == name, m == ''MonadReader ]

isRelevant keys t = isAcceptableContext t && any (`elem` keys) (getStateKeys t)

isAcceptableContext (AppT r r') = isAcceptableContext r && isAcceptableContext r'
isAcceptableContext (ConT con) = con `notElem` [''MonadState, ''MonadReader]
isAcceptableContext _ = True



requireSimpleCon :: Name -> Info -> [Name]
requireSimpleCon _ (TyConI (DataD _ _ names _ _derv)) = names
requireSimpleCon _ (TyConI (NewtypeD _ _ names _ _derv)) = names
requireSimpleCon _ (TyConI (TySynD _ names _)) = names
requireSimpleCon name _ = error $ "Cannot create component from '"++pprint name++"'. Expected a data structure."
