{-# LANGUAGE UndecidableInstances, OverlappingInstances, FlexibleInstances,
             MultiParamTypeClasses, TemplateHaskell, RankNTypes,
             FunctionalDependencies, DeriveDataTypeable,
             GADTs, CPP, ScopedTypeVariables #-}


{- |
Description:

An efficient implementation of queryable sets.

Assume you have a type like:

 @data Entry = Entry Author [Author] Updated Id Content
  newtype Updated = Updated EpochTime
  newtype Id = Id Int64
  newtype Content = Content String
  newtype Author = Author Email
  type Email = String@

1. Decide what parts of your type you want indexed and
   make your type an instance of 'Indexable'

  @instance Indexable Entry () where
    empty = ixSet 
                [ Ix (Map.empty::Map Author (Set Entry)) -- out of order
                , Ix (Map.empty::Map Id (Set Entry))
                , Ix (Map.empty::Map Updated (Set Entry))
                , Ix (Map.empty::Map Test (Set Entry))   -- bogus index
                , Ix (Map.empty::Map Word (Set Entry))   -- text index
                ]
    calcs entry = () -- words for text indexing purposes @

3. Use 'insert', 'delete', 'updateIx', 'deleteIx' and 'empty' to build
   up an 'IxSet' collection

    @entries = foldr insert empty [e1,e2,e3,e4]@
    @entries' = foldr delete entries [e1,e3]@
    @entries'' = update e4 e5 entries@

4. Use the query functions below to grab data from it.  e.g.

     @entries \@< (Updated t1) \@= (Author \"john\@doe.com\")@

  will find all items in entries updated earlier than @t1@ by
  @john\@doe.com@.

5. Text Index

If you want to do add a text index extract the words in entry and pass
them in the 'calc' method of the 'Indexable' class.  Then if you want
all entries with either @word1@ or @word2@, you change the instance to

    @getWords entry = let Just (Content s) =
                           gGet entry in map Word $ words s@

    @instance Indexable Entry [Word] where
    ....
    calcs entry = getWords entry@

Now you can do this query to find entries with any of the words

   @entries \@+ [Word \"word1\",Word \"word2\"]@

And if you want all entries with both:

   @entries \@* [Word \"word1\",Word \"word2\"]@

6. Find only the first author

If an @Entry@ has multiple authors and you want to be able to query on
the first author, define a @FirstAuthor@ datatype and add it to the
result of 'calc'.  @calc e = (toWords e, getFirstAuthor e)@ and now
you can do

   @newtype FirstAuthor = FirstAuthor Email@
   
   @getFirstAuthor = let Just (Author a) = 
                          gGet Entry in FirstAuthor a@

   @instance Indexable Entry ([Word],FirstAuthor)
    ...
    empty = ....
             Ix (Map.empty::Map FirstAuthor (Set Entry))]
    calcs entry = (getWords Entry,getFirstAuthor entry)

    entries \@= (FirstAuthor \"john\@doe.com\")  -- guess what this does@

-}

module Happstack.Data.IxSet 
    (
     module Ix,
         
     -- * Set type
     IxSet,
     Indexable(..),
     noCalcs,
     inferIxSet,
     ixSet,
               
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

     -- * Debugging and optimisation
     stats
)
where

import qualified Happstack.Data.IxSet.Ix as Ix
import           Happstack.Data.IxSet.Ix (Ix(Ix))
import Data.Generics (Data, gmapQ)
import Data.Maybe
import Data.Monoid
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import Happstack.Util.Common
import Control.Monad.Reader
import Language.Haskell.TH as TH

import Happstack.Util.TH
import Happstack.Data
import qualified Data.Generics.SYB.WithClass.Basics as SYBWC
import Prelude hiding (null)

-- the core datatypes

data IxSet a = IxSet [Ix a]
    deriving (Data, Typeable)

-- | Create an 'IxSet' using list of indices. Useful in 'Indexable'
-- 'empty' method.
ixSet :: [Ix a] -> IxSet a
ixSet = IxSet

showTypeOf :: (Typeable a) => a -> String
showTypeOf x = showsPrec 11 (typeOf x) []

instance (Eq a,Ord a,Typeable a) => Eq (IxSet a) where
    IxSet (Ix a:_) == IxSet (Ix b:_) = 
        case cast b of
          Just b' -> a==b'
          Nothing -> error "trying to compare two sets with different types of first indices, this is a bug in library"
    _ == _ = error "comparing sets without indices, this is a bug in library"

instance (Eq a,Ord a,Typeable a) => Ord (IxSet a) where
    compare a b = compare (toSet a) (toSet b)

instance Version (IxSet a)
instance (Serialize a, Ord a, Data a, Indexable a b) => Serialize (IxSet a) where
    putCopy = contain . safePut . toList
    getCopy = contain $ liftM fromList safeGet

instance (SYBWC.Data ctx a, SYBWC.Sat (ctx (IxSet a)), SYBWC.Sat (ctx [a]),
          Indexable a b, Data a, Ord a)
       => SYBWC.Data ctx (IxSet a) where
    gfoldl _ f z ixset  = z fromList `f` toList ixset
    toConstr _ (IxSet _) = ixSetConstr
    gunfold _ k z c  = case SYBWC.constrIndex c of
                       1 -> k (z fromList)
                       _ -> error "IxSet.SYBWC.Data.gunfold unexpected match"
    dataTypeOf _ _ = ixSetDataType

ixSetConstr :: SYBWC.Constr
ixSetConstr = SYBWC.mkConstr ixSetDataType "IxSet" [] SYBWC.Prefix
ixSetDataType :: SYBWC.DataType
ixSetDataType = SYBWC.mkDataType "IxSet" [ixSetConstr]



instance (Indexable a b, Data a, Ord a, Default a) => Default (IxSet a) where
    defaultValue = empty

instance (Ord a,Show a) => Show (IxSet a) where 
    showsPrec prec = showsPrec prec . toSet

instance (Ord a,Read a,Data a,Indexable a b) => Read (IxSet a) where
    readsPrec n = mapFst fromSet . readsPrec n

{- | 'Indexable' class defines objects that can be members of 'IxSet'. 
     If you don't want calculated values use @'Indexable' a ()@.
-}
class (Data b) => Indexable a b | a -> b where
    -- | Method 'empty' defines what an empty 'IxSet' for this
    -- particular type should look like.  It should have all necessary
    -- indices. Use 'ixSet' function to create the set.
    empty :: IxSet a
    -- | Method 'calcs' adds indexable values not found in the
    -- type. Those end up in indices just like other types found in
    -- objects. If you don't want any calculated values just use
    -- 'noCalcs'.
    calcs :: a -> b
        --should this be a fromDyn so we can provide a default impl?
           
-- | Function to be used for 'calcs' in the case of an @'Indexable' a ()@
-- instance.
noCalcs :: t -> ()
noCalcs _ = ()

{- | Template Haskell helper function for automatically building an
   'Indexable' instance from a data type, e.g.

   @data Foo = Foo Int String@ 
   
   and
   
   @$(inferIxSet \"FooDB\" ''Foo 'noCalcs [''Int,''String])@ 
   
   will build a type synonym 

   @type FooDB = IxSet Foo@ 
   
   with @Int@ and @String@ as indices.

   WARNING: The type specified as the first index must be a type which
   appears in all values in the 'IxSet' or 'toList', 'toSet' and
   serialization will not function properly. You will be warned not to do
   this by runtime error.  You can always use the element type
   itself. For example:

   @$(inferIxSet \"FooDB\" ''Foo 'noCalcs [''Foo, ''Int, ''String])@

-} 
inferIxSet :: String -> TH.Name -> TH.Name -> [TH.Name] -> Q [Dec]
inferIxSet _ _ _ [] = error "inferIxSet needs at least one index"
inferIxSet ixset typeName calName entryPoints
    = do calInfo <- reify calName
         typeInfo <- reify typeName
         let (context,binders) = case typeInfo of
                                 TyConI (DataD ctxt _ nms _ _) -> (ctxt,nms)
                                 TyConI (NewtypeD ctxt _ nms _ _) -> (ctxt,nms)
                                 TyConI (TySynD _ nms _) -> ([],nms)
                                 _ -> error "IxSet.inferIxSet typeInfo unexpected match"

             names = map tyVarBndrToName binders

             typeCon = foldl appT (conT typeName) (map varT names)
         case calInfo of
           VarI _ t _ _ ->
               let calType = getCalType t
                   getCalType (ForallT _names _ t') = getCalType t'
                   getCalType (AppT (AppT ArrowT _) t') = t'
                   getCalType t' = error ("Unexpected type in getCalType: " ++ pprint t')
                   mkEntryPoint n = appE (conE 'Ix) (sigE (varE 'Map.empty) (forallT binders (return context) $
                                                                             appT (appT (conT ''Map) (conT n)) (appT (conT ''Set) typeCon)))
               in do i <- instanceD' (return context) (appT (appT (conT ''Indexable) typeCon) (return calType))
                          [d| empty :: IxSet a
                              empty = ixSet $(listE (map mkEntryPoint entryPoints))
                              calcs :: a -> b
                              calcs = $(varE calName) |]
                     let ixType = appT (conT ''IxSet) typeCon
                     ixType' <- tySynD (mkName ixset) binders ixType
                     return $ [i, ixType']  -- ++ d
           _ -> error "IxSet.inferIxSet calInfo unexpected match"

#if MIN_VERSION_template_haskell(2,4,0)
tyVarBndrToName :: TyVarBndr -> Name
tyVarBndrToName (PlainTV nm) = nm
tyVarBndrToName (KindedTV nm _) = nm
#else
tyVarBndrToName :: a -> a
tyVarBndrToName = id
#endif



-- modification operations

type IndexOp =
    forall k a. (Ord k,Ord a) => k -> a -> Map k (Set a) -> Map k (Set a)

-- | Generically traverses the argument, finds all occurences of
-- values of type b and returns them as a list.
--
-- This function properly handles 'String' as 'String' not as @['Char']@.
flatten :: (Typeable a, Data a, Typeable b) => a -> [b]
flatten x = case cast x of
              Just y -> case cast (y :: String) of
                          Just v -> [v]
                          Nothing -> []
              Nothing -> case cast x of
                           Just v -> v : concat (gmapQ flatten x)
                           Nothing -> concat (gmapQ flatten x)

-- | Higher order operator for modifying 'IxSet's.  Use this when your
-- final function should have the form @a -> IxSet a -> IxSet a@,
-- e.g. 'insert' or 'delete'.
change :: (Data a, Ord a,Data b,Indexable a b) =>
          IndexOp -> a -> IxSet a -> IxSet a
change op x (IxSet indices) = 
    IxSet v
    where
    v = zipWith update (True:repeat False) indices
    a = (x,calcs x)
    update firstindex (Ix index) = Ix index'
        where
        key = (undefined :: Map key (Set a) -> key) index
        ds = flatten a
        ii dkey = op dkey x
        index' = if firstindex && List.null ds
                 then error $ "Happstack.Data.IxSet.change: all values must appear in first declared index " ++ showTypeOf key ++ " of " ++ showTypeOf x
                 else foldr ii index ds -- handle multiple values

insertList :: (Data a, Ord a,Data b,Indexable a b) 
           => [a] -> IxSet a -> IxSet a
insertList xs (IxSet indices) = 
    IxSet v
    where
    v = zipWith update (True:repeat False) indices
    as = [(x,calcs x) | x <- xs]
    update firstindex (Ix index) = Ix index'
        where
        key = (undefined :: Map key (Set a) -> key) index
        flattencheck x
            | firstindex = case flatten x of
                             [] -> error $ "Happstack.Data.IxSet.change: all values must appear in first declared index " ++ showTypeOf key ++ " of " ++ showTypeOf x
                             res -> res
            | otherwise = flatten x
        dss = [(k,a) | (a,ca) <- as, k <- flattencheck (a,ca)]
        index' = Ix.insertList dss index

insertMapOfSets :: (Data a, Ord a,Data b,Indexable a b,Typeable key,Ord key) 
                => Map key (Set a) -> IxSet a -> IxSet a
insertMapOfSets originalindex (IxSet indices) = 
    IxSet v
    where
    v = map update indices
    as = [(x,calcs x) | x <- concatMap Set.toList (Map.elems originalindex)]
    update (Ix index) = Ix index'
        where
        dss = [(k,a) | (a,ca) <- as, k <- flatten (a,ca)]
        {- We try to be really clever here. The originalindex is a Map of Sets
           from original index. We want to reuse it as much as possible. If there
           was a guarantee that each element is present at at most one index we
           could reuse originalindex as it is. But there can be more, so we need to
           add remaining ones. Anyway we try to reuse old structure and keep 
           new allocations low as much as possible.
         -}
        index' = case cast originalindex of
                   Just originalindex' -> 
                       let dssf = filter (\(k,v) -> not (Map.member k originalindex')) dss
                       in Ix.insertList dssf originalindex'
                   Nothing -> Ix.insertList dss index

-- | Inserts an item into the 'IxSet'. If your data happens to have
-- primary key this function might not be what you want. See
-- 'updateIx'.
insert :: (Data a, Ord a,Data b,Indexable a b) => a -> IxSet a -> IxSet a
insert = change Ix.insert

-- | Removes an item from the 'IxSet'.
delete :: (Data a, Ord a,Data b,Indexable a b) => a -> IxSet a -> IxSet a
delete = change Ix.delete

-- | Will replace the item with index k.  Only works if there is at
-- most one item with that index in the 'IxSet'. Will not change
-- 'IxSet' if you have more then 1 item with given index.
updateIx :: (Indexable a b, Ord a, Data a, Typeable k)
         => k -> a -> IxSet a -> IxSet a
updateIx i new ixset = insert new $
                     maybe ixset (flip delete ixset) $
                     getOne $ ixset @= i

-- | Will delete the item with index k.  Only works if there is at
-- most one item with that index in the 'IxSet'. Will not change
-- 'IxSet' if you have more then 1 item with given index.
deleteIx :: (Indexable a b, Ord a, Data a, Typeable k)
         => k -> IxSet a -> IxSet a
deleteIx i ixset = maybe ixset (flip delete ixset) $
                       getOne $ ixset @= i

-- conversion operations

-- | Converts an 'IxSet' to a 'Set' of its elements.
toSet :: Ord a => IxSet a -> Set a
toSet (IxSet (Ix ix:_)) = Map.fold Set.union Set.empty ix
toSet (IxSet []) = Set.empty

-- | Converts a 'Set' to an 'IxSet'.
fromSet :: (Indexable a b, Ord a, Data a) => Set a -> IxSet a
fromSet = fromList . Set.toList

-- | Converts a list to an 'IxSet'.
fromList :: (Indexable a b, Ord a, Data a) => [a] -> IxSet a
fromList list = insertList list empty

-- | Returns the number of unique items in the 'IxSet'.
size :: Ord a => IxSet a -> Int
size = Set.size . toSet

-- | Converts an 'IxSet' to its list of elements.
toList :: Ord a => IxSet a -> [a]
toList = Set.toList . toSet

-- | If the 'IxSet' is a singleton it will return the one item stored in it.
-- If 'IxSet' is empty or has many elements this function returns 'Nothing'.
getOne :: Ord a => IxSet a -> Maybe a
getOne ixset = case toList ixset of
                   [x] -> Just x
                   _   -> Nothing

-- | Like 'getOne' with a user provided default.
getOneOr :: Ord a => a -> IxSet a -> a
getOneOr def = fromMaybe def . getOne

-- | Return 'True' if the 'IxSet' is empty, 'False' otherwise.
null :: IxSet a -> Bool
null (IxSet (Ix ix:_)) = Map.null ix
null (IxSet [])        = True

-- set operations

-- | An infix 'intersection' operation.
(&&&) :: (Ord a, Data a, Indexable a b) => IxSet a -> IxSet a -> IxSet a
(&&&) = intersection

-- | An infix 'union' operation.
(|||) :: (Ord a, Data a, Indexable a b) => IxSet a -> IxSet a -> IxSet a
(|||) = union

infixr 5 &&&
infixr 5 |||

-- | Takes the union of the two 'IxSet's.
union :: (Ord a, Data a, Indexable a b) => IxSet a -> IxSet a -> IxSet a
union (IxSet x1) (IxSet x2) = IxSet indices'
    where
      indices' = zipWith union' x1 x2
      union' (Ix a) (Ix b) = 
          case cast b of
            Nothing -> error "IxSet.union: indexes out of order"
            Just b' -> Ix (Ix.union a b')

-- | Takes the intersection of the two 'IxSet's.
intersection :: (Ord a, Data a, Indexable a b) => IxSet a -> IxSet a -> IxSet a
intersection (IxSet x1) (IxSet x2) = IxSet indices'
    where
      indices' = zipWith intersection' x1 x2
      intersection' (Ix a) (Ix b) = 
          case cast b of
            Nothing -> error "IxSet.intersection: indexes out of order"
            Just b' -> Ix (Ix.intersection a b')


-- query operators

-- | Infix version of 'getEQ'.
(@=) :: (Indexable a b, Data a, Ord a, Typeable k)
     => IxSet a -> k -> IxSet a
ix @= v = getEQ v ix

-- | Infix version of 'getLT'.
(@<) :: (Indexable a b, Data a, Ord a, Typeable k)
     => IxSet a -> k -> IxSet a
ix @< v = getLT v ix

-- | Infix version of 'getGT'.
(@>) :: (Indexable a b, Data a, Ord a, Typeable k)
     => IxSet a -> k -> IxSet a
ix @> v = getGT v ix

-- | Infix version of 'getLTE'.
(@<=) :: (Indexable a b, Data a, Ord a, Typeable k)
      => IxSet a -> k -> IxSet a
ix @<= v = getLTE v ix

-- | Infix version of 'getGTE'.
(@>=) :: (Indexable a b, Data a, Ord a, Typeable k)
      => IxSet a -> k -> IxSet a
ix @>= v = getGTE v ix

-- | Returns the subset with indices in the open interval (k,k).
(@><) :: (Indexable a b, Data a, Ord a, Typeable k)
      => IxSet a -> (k, k) -> IxSet a
ix @>< (v1,v2) = getLT v2 $ getGT v1 ix

-- | Returns the subset with indices in [k,k).
(@>=<) :: (Indexable a b, Data a, Ord a, Typeable k)
       => IxSet a -> (k, k) -> IxSet a
ix @>=< (v1,v2) = getLT v2 $ getGTE v1 ix

-- | Returns the subset with indices in (k,k].
(@><=) :: (Indexable a b, Data a, Ord a, Typeable k)
       => IxSet a -> (k, k) -> IxSet a
ix @><= (v1,v2) = getLTE v2 $ getGT v1 ix

-- | Returns the subset with indices in [k,k].
(@>=<=) :: (Indexable a b, Data a, Ord a, Typeable k)
        => IxSet a -> (k, k) -> IxSet a
ix @>=<= (v1,v2) = getLTE v2 $ getGTE v1 ix

-- | Creates the subset that has an index in the provided list.
(@+) :: (Indexable a b, Data a, Ord a, Typeable k)
     => IxSet a -> [k] -> IxSet a
ix @+ list = foldr union empty        $ map (ix @=) list

-- | Creates the subset that matches all the provided indices.
(@*) :: (Indexable a b, Data a, Ord a, Typeable k)
     => IxSet a -> [k] -> IxSet a
ix @* list = foldr intersection empty $ map (ix @=) list

-- | Returns the subset with an index equal to the provided key.  The
-- set must be indexed over key type, doing otherwise results in
-- runtime error.
getEQ :: (Indexable a b, Data a, Ord a, Typeable k)
      => k -> IxSet a -> IxSet a
getEQ = getOrd EQ

-- | Returns the subset with an index less than the provided key.  The
-- set must be indexed over key type, doing otherwise results in
-- runtime error.
getLT :: (Indexable a b, Data a, Ord a, Typeable k)
      => k -> IxSet a -> IxSet a
getLT = getOrd LT

-- | Returns the subset with an index greater than the provided key.
-- The set must be indexed over key type, doing otherwise results in
-- runtime error.
getGT :: (Indexable a b, Data a, Ord a, Typeable k)
      => k -> IxSet a -> IxSet a
getGT = getOrd GT

-- | Returns the subset with an index less than or equal to the
-- provided key.  The set must be indexed over key type, doing
-- otherwise results in runtime error.
getLTE :: (Indexable a b, Data a, Ord a, Typeable k)
       => k -> IxSet a -> IxSet a
getLTE = getOrd2 True True False

-- | Returns the subset with an index greater than or equal to the
-- provided key.  The set must be indexed over key type, doing
-- otherwise results in runtime error.
getGTE :: (Indexable a b, Data a, Ord a, Typeable k)
       => k -> IxSet a -> IxSet a
getGTE = getOrd2 False True True

-- | Returns the subset with an index within the interval provided.
-- The bottom of the interval is closed and the top is open,
-- i. e. [k1;k2).  The set must be indexed over key type, doing
-- otherwise results in runtime error.
getRange :: (Indexable a b, Typeable k, Ord a, Data a)
         => k -> k -> IxSet a -> IxSet a
getRange k1 k2 ixset = getGTE k1 (getLT k2 ixset)

-- | Returns lists of elements paired with the indices determined by
-- type inference.
groupBy::(Typeable k,Typeable t) =>  IxSet t -> [(k, [t])]
groupBy (IxSet indices) = collect indices
    where
    collect [] = [] -- FIXME: should be an error
    collect (Ix index:is) = maybe (collect is) f (cast index)
    f = mapSnd Set.toList . Map.toList
    
--query impl function

-- | A function for building up selectors on 'IxSet's.  Used in the
-- various get* functions.  The set must be indexed over key type,
-- doing otherwise results in runtime error.

getOrd :: (Indexable a b, Ord a, Data a, Typeable k)
       => Ordering -> k -> IxSet a -> IxSet a
getOrd LT = getOrd2 True False False
getOrd EQ = getOrd2 False True False
getOrd GT = getOrd2 False False True

-- | A function for building up selectors on 'IxSet's.  Used in the
-- various get* functions.  The set must be indexed over key type,
-- doing otherwise results in runtime error.
getOrd2 :: (Indexable a b, Ord a, Data a, Typeable k)
        => Bool -> Bool -> Bool -> k -> IxSet a -> IxSet a
getOrd2 inclt inceq incgt v ixset@(IxSet indices) = collect indices
    where
    collect [] = error $ "IxSet: there is no index " ++ showTypeOf v ++ 
                 " in " ++ showTypeOf ixset
    collect (Ix index:is) = maybe (collect is) f $ cast v
        where
        f v'' = insertMapOfSets result empty
            where
            (lt',eq',gt') = Map.splitLookup v'' index
            ltgt = Map.unionWith Set.union lt gt
            result = case eq of
                       Just eqset -> Map.insertWith Set.union v'' eqset ltgt
                       Nothing -> ltgt                     
            lt = if inclt 
                 then lt'
                 else Map.empty
            gt = if incgt 
                 then gt'
                 else Map.empty
            eq = if inceq
                 then eq'
                 else Nothing

--we want a gGets that returns a list of all matches

{--
Optimization todo:

* can we avoid rebuilding the collection every time we query?
  does laziness take care of everything?

* nicer operators?

* nice way to do updates that doesn't involve reinserting the entire data

* can we index on xpath rather than just type?

--}

instance (Indexable a b, Data a, Ord a) => Monoid (IxSet a) where
    mempty = empty
    mappend = union

-- | Statistics about 'IxSet'. This function returns quadruple
-- consisting of 1. total number of elements in the set 2. number of
-- declared indices 3. number of keys in all indices 4. number of
-- values in all keys in all indices. This can aid you in debugging
-- and optimisation.
stats :: (Ord a) => IxSet a -> (Int,Int,Int,Int)
stats (IxSet indices) = (no_elements,no_indices,no_keys,no_values)
    where
      no_elements = size (IxSet indices)
      no_indices = length indices
      no_keys = sum [Map.size m | Ix m <- indices]
      no_values = sum [sum [Set.size s | s <- Map.elems m] | Ix m <- indices]

