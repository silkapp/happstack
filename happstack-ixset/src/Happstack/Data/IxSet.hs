{-# LANGUAGE ScopedTypeVariables #-}
module Happstack.Data.IxSet 
    (
     -- * Set type
     IxSet,
     Indexable(..),
     noCalcs,
     inferIxSet,
     ixSet,
     ixFun,
     ixGen,
               
     -- * Changes to set
     IndexOp,
     change,
     insert,
     delete,
     updateIx,
     deleteIx,

     -- * Creation
     fromSet,
     fromList,

     -- * Conversion
     toSet,
     toList,
     toAscList,
     toDescList,
     getOne,
     getOneOr,

     -- * Size checking
     size,
     null,

     -- * Set operations
     (&&&),
     (|||),
     union,
     intersection,

     -- * Indexing
     (@=),
     (@<),
     (@>),
     (@<=),
     (@>=),
     (@><),
     (@>=<),
     (@><=),
     (@>=<=),
     (@+),
     (@*),
     getEQ,
     getLT,
     getGT,
     getLTE,
     getGTE,
     getRange,
     groupBy,
     groupAscBy,
     groupDescBy,

     -- * Index creation helpers
     flatten,
     flattenWithCalcs,

     -- * Debugging and optimisation
     stats
)
where

import Prelude hiding (null)
import Data.Data
import Data.IxSet hiding (ixGen, toAscList, toDescList, Proxy)
import qualified Data.IxSet as S 
import Data.IxSet.Ix (Ix)
import qualified Happstack.Data as H
import           Happstack.Data hiding (Proxy)

-- | Create a generic index. Provided example is used only as type
-- source so you may use a 'Proxy'. The 'ixGen' uses flatten to
-- traverse value using its 'Data' instance.
--
-- > instance Indexable Type where
-- >     empty = ixSet [ ixGen (Proxy :: Proxy Type) ]
--
-- In production systems consider using 'ixFun' in place of 'ixGen' as
-- the former one is much faster.
ixGen :: forall a b . (Data a, Ord b, Typeable b) => H.Proxy b -> Ix a
ixGen _example = S.ixGen (S.Proxy :: S.Proxy b)

-- | Converts an 'IxSet' to its list of elements.
--
-- List will be sorted in ascending order by the index 'k'.
--
-- The list may contain duplicate entries if a single value produces multiple keys.
toAscList :: forall k a. (Indexable a, Typeable a, Typeable k) => H.Proxy k -> IxSet a -> [a]
toAscList _ ixset = S.toAscList (S.Proxy :: S.Proxy k) ixset

-- | Converts an 'IxSet' to its list of elements.
--
-- List will be sorted in descending order by the index 'k'.
--
-- The list may contain duplicate entries if a single value produces multiple keys.
toDescList :: forall k a. (Indexable a, Typeable a, Typeable k) => H.Proxy k -> IxSet a -> [a]
toDescList _ ixset = S.toDescList (S.Proxy :: S.Proxy k) ixset

instance Version (IxSet a)
instance (Serialize a, Ord a, Typeable a, Indexable a) => Serialize (IxSet a) where
    putCopy = contain . safePut . toList
    getCopy = contain $ fmap fromList safeGet

instance (Indexable a, Ord a,Data a, Default a) => Default (IxSet a) where
    defaultValue = empty


