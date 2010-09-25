{-# LANGUAGE FlexibleInstances, TemplateHaskell, CPP, PatternGuards #-}
module Happstack.State.ComponentTH
    ( mkMethods
    ) where

import Data.Char
import Language.Haskell.TH

import Happstack.State.Types
import Happstack.Data.Serialize
import Happstack.State.ComponentSystem

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Maybe

import Data.List

import Data.Generics.Basics

#if MIN_VERSION_template_haskell(2,4,0)
type CtxElemQ = PredQ
type CtxElem  = Pred
#else
type CtxElemQ = TypeQ
type CtxElem  = Type
#endif

nubCxt :: [CtxElemQ] -> Q [CtxElem]
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

{- | This function will derive, for the provided type name and the provided
   list of methods, an instance of Methods for the given type and necessary
   instance of UpdateEvent and QueryEvent to make the provided list of functions
   into Methods.
   This TH splice needs to be executed for any MACID state type in order to 
   have the necessary code for event handling generated.
   Ex:  @$(mkMethods ''Foo ['fooUpdate,'fooQuery'])@
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

mkKeyConstraints :: [Name] -> [CtxElemQ]
mkKeyConstraints keys
    = [ mkCtxt ''Typeable (varT key) | key <- keys ] ++
      [ mkCtxt ''Serialize (varT key) | key <- keys ]

mkMethodConstraints :: [Name] -> MethodInfo -> [CtxElemQ]
mkMethodConstraints keys method
    = map return (substMethodContext method keys)

substMethodContext :: MethodInfo -> [Name] -> [CtxElem]
substMethodContext method keys
    = let relation = zip (methodKeys method) keys

          workerT (VarT old) | Just new <- lookup old relation
                   = VarT new
          workerT (AppT l r) = AppT (workerT l) (workerT r)
          workerT (ForallT c names t) = ForallT c names (workerT t)
          workerT t = t

#if MIN_VERSION_template_haskell(2,4,0)
          workerP (ClassP old typs)
              = let new = case lookup old relation of
                            Nothing -> old
                            Just nm -> nm
                in ClassP new $ map workerT typs
          workerP p = p

          worker = workerP
#else
          worker = workerT
#endif

      in map worker (methodContext method)

mkType :: Name -> [Name] -> TypeQ
mkType name args = foldl appT (conT name) (map varT args)

genSerializeInstances :: [MethodInfo] -> Q [Dec]
genSerializeInstances meths
    = liftM concat $ forM meths $ \method ->
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
genComponentHandlers meths
    = do let localHandlers = flip map meths $ \method ->
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
         instanceD (nubCxt $ [mkCtxt ''Serialize eventType
                             ,mkCtxt ''Serialize (return (methodResult method))
                             ]
                          ++ mkKeyConstraints keys
                          ++ mkMethodConstraints keys method
                   )
                   (appT (appT (conT (methodClass method)) eventType) (return (methodResult method)))
                   []

mkCtxt :: Name -> TypeQ -> CtxElemQ
#if MIN_VERSION_template_haskell(2,4,0)
mkCtxt name typ = classP name [typ]
#else
mkCtxt name typ = appT (conT name) typ
#endif

genMethodStructs :: [Name] -> [MethodInfo] -> Q [Dec]
genMethodStructs derv meths
    = liftM concat (mapM (genMethodStruct derv) meths)

-- FIXME: allow class constraints on keys.
genMethodStruct :: [Name] -> MethodInfo -> Q [Dec]
genMethodStruct derv method
    = do let c = NormalC (upperName (methodName method)) (zip (repeat NotStrict ) (methodArgs method))
         return [ DataD [] (upperName (methodName method)) (map mkTyVarBndr $ methodKeys method) [c] (derv) ]

 where

#if MIN_VERSION_template_haskell(2,4,0)
   mkTyVarBndr = PlainTV
#else
   mkTyVarBndr = id
#endif

upperName :: Name -> Name
upperName = mkName . upperFirst . nameBase

upperFirst :: String -> String
upperFirst (x:xs) = toUpper x : xs
upperFirst "" = error "ComponentTH.UpperFirst []"

data MethodInfo = Method { methodName   :: Name
                         , methodKeys   :: [Name]
                         , methodContext:: [CtxElem]
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

showInfo :: Info -> String
showInfo ClassI{} = "class"
showInfo TyConI{} = "type constructor"
showInfo PrimTyConI{} = "primitive type constructor"
showInfo DataConI{} = "data constructor"
showInfo VarI{} = "variable"
showInfo TyVarI{} = "type variable"
showInfo x = pprint x


-- Cases:
--  forall m. MonadState state m => X -> m Y
--  forall key. key -> Update ()
--  forall key m. MonadState state m => key -> m ()
--  X -> Ev (ReaderT state STM) Y
--  X -> Ev (StateT state STM) Y
getTypes :: Type -> MethodInfo
getTypes (ForallT _ contxt t) = getTypes' contxt t
getTypes t = getTypes' [] t

-- FIXME: only allow type variables used by the component.
getTypes' :: Cxt -> Type -> MethodInfo
getTypes' contxt t
    = case runWriter (worker t) of
        ((keys,className, typeName, res), args) -> Method { methodName = error "Method name not set", methodKeys = keys
                                                          , methodContext = filter (isRelevant keys) contxt
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
              | Just state <- isMonadState contxt name  = return (getStateKeys state, ''UpdateEvent, 'Update, r)
              | Just state <- isMonadReader contxt name = return (getStateKeys state, ''QueryEvent, 'Query, r)
          -- error case
          worker c = error ("Unexpected method type: " ++ pprint c)

getStateKeys :: Type -> [Name]
getStateKeys (AppT r r') = getStateKeys r ++ getStateKeys r'
getStateKeys (VarT key) = [key]
getStateKeys (ConT _st) = []
getStateKeys v = error $ "Bad state type: " ++ pprint v ++ " (expected a constant, an application or a type variable)"

-- |Ignoring EqualP predicates, returns all names associated with a
-- context predicate
getPredKeys :: CtxElem -> [Name]
#if MIN_VERSION_template_haskell(2,4,0)
getPredKeys (ClassP nm typs) = nm : concatMap getStateKeys typs
getPredKeys _ = []
#else
getPredKeys = getStateKeys
#endif


isMonadState :: [CtxElem] -> Type -> Maybe Type
isMonadState = isMonadX ''MonadState

isMonadReader :: [CtxElem] -> Type -> Maybe Type
isMonadReader = isMonadX ''MonadReader

isMonadX :: Name -> [CtxElem] -> Type -> Maybe Type
#if MIN_VERSION_template_haskell(2,4,0)
isMonadX monadType contxt name = listToMaybe [ state | ClassP m [state,mName] <- contxt, 
                                         mName == name, m == monadType ]
#else
isMonadX monadType contxt name = listToMaybe [ state | AppT (AppT (ConT m) state) mName <- contxt,
                                         mName == name, m == monadType ]
#endif



isRelevant :: [Name] -> CtxElem -> Bool
isRelevant keys p = isAcceptableContext p && any (`elem` keys) (getPredKeys p)

isAcceptableContext :: CtxElem -> Bool
#if MIN_VERSION_template_haskell(2,4,0)
isAcceptableContext (ClassP con _) = con `notElem` [''MonadState, ''MonadReader] 
#else
isAcceptableContext (AppT r r') = isAcceptableContext r && isAcceptableContext r'
isAcceptableContext (ConT con) = con `notElem` [''MonadState, ''MonadReader]
#endif
isAcceptableContext _ = True

requireSimpleCon :: Name -> Info -> [Name]
requireSimpleCon _ (TyConI (DataD _ _ names _ _derv)) = map conv names
requireSimpleCon _ (TyConI (NewtypeD _ _ names _ _derv)) = map conv names
requireSimpleCon _ (TyConI (TySynD _ names _)) = map conv names
requireSimpleCon name _ = error $ "Cannot create component from '"++pprint name++"'. Expected a data structure."

#if MIN_VERSION_template_haskell(2,4,0)
conv :: TyVarBndr -> Name
conv (PlainTV nm) = nm
conv (KindedTV nm _) = nm
#else
conv = id
#endif