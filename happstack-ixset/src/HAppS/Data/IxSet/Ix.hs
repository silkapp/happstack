{-# LANGUAGE UndecidableInstances, OverlappingInstances, FlexibleInstances,
             MultiParamTypeClasses, TemplateHaskell, PolymorphicComponents,
             DeriveDataTypeable,ExistentialQuantification #-}

module HAppS.Data.IxSet.Ix where

import Data.Generics hiding (GT)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import HAppS.Data
import qualified Data.Generics.SYB.WithClass.Basics as SYBWC

-- the core datatypes

data Ix a = IxDefault |
            forall key . (Typeable key, Ord key) => Ix (Map key (Set a))
    deriving Typeable

 -- minimal hacky instance
instance Data a => Data (Ix a) where
    toConstr (Ix _) = con_Ix_Data
    toConstr _ = error "unexpected match for: toConstr"
    gunfold _ _     = error "gunfold"
    dataTypeOf _    = ixType_Data


con_Ix_Data :: Constr
con_Ix_Data = mkConstr ixType_Data "Ix" [] Prefix
ixType_Data :: DataType
ixType_Data = mkDataType "HAppS.Data.IxSet.Ix" [con_Ix_Data]


instance Default a => Default (Ix a) where
    defaultValue = IxDefault

ixDefaultConstr :: SYBWC.Constr
ixDefaultConstr = SYBWC.mkConstr ixDataType "IxDefault" [] SYBWC.Prefix
ixConstr :: SYBWC.Constr
ixConstr = SYBWC.mkConstr ixDataType "Ix" [] SYBWC.Prefix
ixDataType :: SYBWC.DataType
ixDataType = SYBWC.mkDataType "Ix" [ixDefaultConstr, ixConstr]

instance (SYBWC.Data ctx a, SYBWC.Sat (ctx (Ix a)))
       => SYBWC.Data ctx (Ix a) where
    gfoldl = error "gfoldl Ix"
    toConstr _ IxDefault = ixDefaultConstr
    toConstr _ (Ix _)    = ixConstr
    gunfold = error "gunfold Ix"
    dataTypeOf _ _ = ixDataType

-- modification operations

insert :: (Ord a, Ord k)
       => k -> a -> Map k (Set a) -> Map k (Set a)
insert k v index = Map.insertWith Set.union k (Set.singleton v) index

delete :: (Ord a, Ord k)
       => k -> a -> Map k (Set a) -> Map k (Set a)
delete k v index = Map.update remove k index
    where
    remove set = let set' = Set.delete v set
                 in if Set.null set' then Nothing else Just set'

