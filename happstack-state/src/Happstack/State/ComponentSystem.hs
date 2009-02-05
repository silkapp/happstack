{-# LANGUAGE TypeFamilies, FlexibleContexts, FunctionalDependencies, CPP,
             MultiParamTypeClasses, TypeOperators, DeriveDataTypeable, GADTs #-}
module Happstack.State.ComponentSystem where

import Happstack.Data.Serialize
import Happstack.Data.Proxy
import Happstack.State.Types

import Data.Typeable
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.State.Strict
import qualified Data.ByteString.Lazy.Char8 as L

--------------------------------------------------------------
-- Type level list
-------------------------------------------------------------
data End = End
data h :+: t = h :+: t
infixr 6 :+:


--------------------------------------------------------------
-- Event classes
--------------------------------------------------------------

-- TH doesn't support type families yet.
class (Serialize ev, Serialize res) => UpdateEvent ev res | ev -> res
class (Serialize ev, Serialize res) => QueryEvent ev res | ev -> res

--------------------------------------------------------------
-- Methods contain the query and update handlers for a component
--------------------------------------------------------------

data Method st where
    Update :: (UpdateEvent ev res) => (ev -> Update st res) -> Method st
    Query  :: (QueryEvent ev res) => (ev -> Query st res) -> Method st

instance Show (Method st) where
    show method = "Method: " ++ methodType method


methodType :: Method t -> String
methodType m = case m of
                Update fn -> let ev :: (ev -> Update st res) -> ev
                                 ev _ = undefined
                             in show (typeOf (ev fn))
                Query fn  -> let ev :: (ev -> Query st res) -> ev
                                 ev _ = undefined
                             in show (typeOf (ev fn))

class Methods a where
    methods :: Proxy a -> [Method a]

data MethodMap where
    MethodMap :: (Component st) => Map String (Method st) -> MethodMap

instance Show MethodMap where
    show (MethodMap m) = show m

-- State type -> method map
type ComponentTree = Map String MethodMap

-- State type -> all versions
type ComponentVersions = Map String [L.ByteString]

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

--------------------------------------------------------------
-- Class for walking the component tree.
--------------------------------------------------------------
class SubHandlers a where
    subHandlers :: a -> Collect ()

instance SubHandlers End where
    subHandlers ~End = return ()
instance (Methods a, Component a, SubHandlers b) => SubHandlers (a :+: b) where
    subHandlers ~(a :+: b) = do collectHandlers' (proxy a)
                                subHandlers b


data Collection = Collection ComponentTree ComponentVersions [IO ()]

addItem :: (MonadState Collection m) => 
           String -> MethodMap -> [L.ByteString] -> IO () -> m ()
addItem key item versions ioAction
    = do Collection tree allVersions ioActions <- get
         case Map.member key tree of
           False -> put $ Collection (Map.insert key item tree) (Map.insert key versions allVersions) (ioAction:ioActions)
           True  -> dup key

type Collect = State Collection

collectHandlers :: (Methods a, Component a) => Proxy a -> (ComponentTree, ComponentVersions, [IO ()])
collectHandlers prox
    = case execState (collectHandlers' prox) (Collection Map.empty Map.empty []) of
        Collection tree versions ioActions -> (tree, versions, ioActions)

collectHandlers' :: (Methods a, Component a) => Proxy a -> Collect ()
collectHandlers' prox
    = let key = show (typeOf (unProxy prox))
          item = MethodMap $ Map.fromList [ (methodType m, m) | m <- methods prox ]
          versions = collectVersions prox
      in do addItem key item versions (onLoad prox)
            subHandlers (sub prox)
    where sub :: Component a => Proxy a -> Dependencies a
          sub _ = undefined

dup :: String -> b
dup key = error $ "Duplicate component: " ++ key
