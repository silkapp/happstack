{-# LANGUAGE TypeFamilies, FlexibleContexts, FunctionalDependencies, CPP,
             MultiParamTypeClasses, TypeOperators, DeriveDataTypeable, GADTs #-}
module HAppS.State.ComponentSystem where

import HAppS.Data.Serialize
import HAppS.Data.Proxy
import HAppS.State.Types

import Data.Typeable
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.State.Strict
import qualified Data.ByteString.Lazy.Char8 as L

--------------------------------------------------------------
-- Type level list
-------------------------------------------------------------
#ifndef __HADDOCK__
data End = End
data h :+: t = h :+: t
infixr 6 :+:
#endif


--------------------------------------------------------------
-- Event classes
--------------------------------------------------------------

-- TH doesn't support type families yet.
class (Serialize ev, Serialize res) => UpdateEvent ev res | ev -> res
class (Serialize ev, Serialize res) => QueryEvent ev res | ev -> res

--------------------------------------------------------------
-- Methods contain the query and update handlers for a component
--------------------------------------------------------------

#ifndef __HADDOCK__
data Method st where
    Update :: (UpdateEvent ev res) => (ev -> Update st res) -> Method st
    Query  :: (QueryEvent ev res) => (ev -> Query st res) -> Method st
#endif

instance Show (Method st) where
    show method = "Method: " ++ methodType method


methodType m = case m of
                Update fn -> let ev :: (ev -> Update st res) -> ev
                                 ev _ = undefined
                             in show (typeOf (ev fn))
                Query fn  -> let ev :: (ev -> Query st res) -> ev
                                 ev _ = undefined
                             in show (typeOf (ev fn))

class Methods a where
    methods :: Proxy a -> [Method a]

#ifndef __HADDOCK__
data MethodMap where
    MethodMap :: (Component st) => Map String (Method st) -> MethodMap
#endif

instance Show MethodMap where
    show (MethodMap m) = show m

-- State type -> method map
type ComponentTree = Map String MethodMap

-- State type -> all versions
type ComponentVersions = Map String [L.ByteString]

#ifndef __HADDOCK__
--------------------------------------------------------------
-- A component consists of a list of subcomponents, some
-- initiation code and some methods.
-- The methods are in a different class so they can be generated
-- separately.
--------------------------------------------------------------
class (SubHandlers (Dependencies a),Serialize a) => Component a where
    type Dependencies a
    initialValue :: a
    onLoad :: Proxy a -> IO ()
    onLoad _ = return ()
#endif

--------------------------------------------------------------
-- Class for walking the component tree.
--------------------------------------------------------------
class SubHandlers a where
    subHandlers :: a -> Collect ()
#ifndef __HADDOCK__
instance SubHandlers End where
    subHandlers ~End = return ()
instance (Methods a, Component a, SubHandlers b) => SubHandlers (a :+: b) where
    subHandlers ~(a :+: b) = do collectHandlers' (proxy a)
                                subHandlers b
#endif

data Collection = Collection ComponentTree ComponentVersions [IO ()]

addItem key item versions ioAction
    = do Collection tree allVersions ioActions <- get
         case Map.member key tree of
           False -> put $ Collection (Map.insert key item tree) (Map.insert key versions allVersions) (ioAction:ioActions)
           True  -> dup key

type Collect = State Collection

collectHandlers :: (Methods a, Component a) => Proxy a -> (ComponentTree, ComponentVersions, [IO ()])
collectHandlers proxy
    = case execState (collectHandlers' proxy) (Collection Map.empty Map.empty []) of
        Collection tree versions ioActions -> (tree, versions, ioActions)

collectHandlers' :: (Methods a, Component a) => Proxy a -> Collect ()
collectHandlers' proxy
    = let key = show (typeOf (unProxy proxy))
          item = MethodMap $ Map.fromList [ (methodType m, m) | m <- methods proxy ]
          versions = collectVersions proxy
      in do addItem key item versions (onLoad proxy)
            subHandlers (sub proxy)
    where sub :: Component a => Proxy a -> Dependencies a
          sub _ = undefined

dup key = error $ "Duplicate component: " ++ key




--------------------------------------------------------------
-- Tests
--------------------------------------------------------------
{-

data Test = Test deriving (Typeable)
instance StartState Test where startState = Test
instance Serialize Test

instance Methods Test where
    methods _ = []

instance Component Test where
    -- Note that subcomponents are separate from the state.
    -- This means we have no requirements to the format of the state.
    type Depdendencies Test = UniqueSupply Int :+:
                              UniqueSupply String :+:
                              End
    onLoad _ = putStrLn "test init"



data UniqueSupply a = UniqueSupply Int deriving Typeable
instance StartState (UniqueSupply a) where startState = UniqueSupply 0
instance Typeable a => Serialize (UniqueSupply a)

-- This instance and the associated data types are usually generated by TH.
-- However, it can also be written manually if a show-stopper flaw was
-- encountered in mentioned TH code.
instance (Show a, Ord a) => Methods (UniqueSupply a) where
    methods _ = []

-- Note that 'Show' and 'Ord' does not have to be mentioned here.
instance (Typeable a) => Component (UniqueSupply a) where
    type Depdendencies (UniqueSupply a) = End
    onLoad proxy = putStrLn $ "Unique init: " ++ show (typeOf $ unProxy proxy)




mainState v = collectHandlers (proxy v)

runTest = let (tree, _, io) = mainState Test
          in do sequence_ io
                print tree
-}
