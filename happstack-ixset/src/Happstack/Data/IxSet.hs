{-# LANGUAGE UndecidableInstances, OverlappingInstances, FlexibleInstances,
             MultiParamTypeClasses, TemplateHaskell, RankNTypes,
             FunctionalDependencies, DeriveDataTypeable,
             GADTs, CPP #-}


{- |
Description:

An efficient implementation of queryable sets.

Assume you have a type like:

  data Entry = Entry Author [Author] Updated Id Content
  newtype Updated = Updated EpochTime
  newtype Id = Id Int64
  newtype Content = Content String
  newtype Author = Author Email
  type Email = String

1. Decide what parts of your type you want indexed, and
   make your type an instance of Indexable

    instance Indexable Entry () where
    empty = IxSet[
                ,Ix (Map.empty::Map Author (Set Entry)) --out of order
                ,Ix (Map.empty::Map Id (Set Entry))
                ,Ix (Map.empty::Map Updated (Set Entry))
                ,Ix (Map.empty::Map Test (Set Entry)) -- bogus index
                ,Ix (Map.empty::Map Word (Set Entry)) -- text index
                ]
    calcs entry = () -- words for text indexing purposes

3. Use insert/delete/replace and empty to build up an IxSet collection

    entries = foldr insert empty [e1,e2,e3,e4]
    entries' = foldr delete entries [e1,e3]
    entries'' = update e4 e5 entries

4. Use the query functions below to grab data from it.  e.g.

     entries @< (Updated t1) @= (Author "john@doe.com")

  will find all items in entries updated earlier than t1 by
  john@doe.com.

5. Text Index

If you want to do add a text index extract the words in entry and pass
them in the calc method of the Indexable class.  Then if you want
all entries with either word1 or word2, you change the instance to

    getWords entry = let Just (Content s) =
                                     gGet entry in map Word $ words s

    instance Indexable Entry [Word] where
    ....
    calcs entry = getWords entry

Now you can do this query to find entries with any of the words

   entries @+ [Word "word1",Word "word2"]

And if you want all entries with both:

   entries @* [Word "word1",Word "word2"]

6. Find the only the first author

If an Entry has multiple authors and you want to be able to query
on the first author, define a FirstAuthor datatype and add it to the
result of calc.  calc e=(toWords e,getFirstAuthor e) and now you can
do

   newtype FirstAuthor = FirstAuthor Email
   getFirstAuthor = let Just (Author a)=gGet Entry in FirstAuthor a

   instance Indexable Entry ([Word],FirstAuthor)
    ...
    empty = ....
             Ix (Map.empty::Map FirstAuthor (Set Entry))]
    calcs entry = (getWords Entry,getFirstAuthor entry)

    entries @= (FirstAuthor "john@doe.com")  -- guess what this does

-}

module Happstack.Data.IxSet (module Happstack.Data.IxSet,
                         module Ix)
    where

import qualified Happstack.Data.IxSet.Ix as Ix
import           Happstack.Data.IxSet.Ix (Ix(Ix))
import Data.Generics hiding (GT)
import Data.Dynamic
import Data.Maybe
import Data.Monoid
import           Data.List (partition)
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

-- the core datatypes

data IxSet a = ISet [a] | IxSet [Ix a]
    deriving (Data, Typeable)

instance Version (IxSet a)
instance (Serialize a, Ord a, Data a, Indexable a b) => Serialize (IxSet a) where
    putCopy = contain . safePut . toList
    getCopy = contain $ liftM fromList safeGet

instance (SYBWC.Data ctx a, SYBWC.Sat (ctx (IxSet a)), SYBWC.Sat (ctx [a]),
          Indexable a b, Data a, Ord a)
       => SYBWC.Data ctx (IxSet a) where
    gfoldl _ f z (IxSet x)  = z fromList `f` toList' x
    gfoldl _ f z (ISet x)   = z ISet `f` x
    toConstr _ (ISet  _) = iSetConstr
    toConstr _ (IxSet _) = ixSetConstr
    gunfold _ k z c  = case SYBWC.constrIndex c of
                       1 -> k (z ISet)
                       2 -> k (z fromList)
                       _ -> error "unexpected match"
    dataTypeOf _ _ = ixSetDataType

iSetConstr :: SYBWC.Constr
iSetConstr = SYBWC.mkConstr ixSetDataType "ISet" [] SYBWC.Prefix
ixSetConstr :: SYBWC.Constr
ixSetConstr = SYBWC.mkConstr ixSetDataType "IxSet" [] SYBWC.Prefix
ixSetDataType :: SYBWC.DataType
ixSetDataType = SYBWC.mkDataType "IxSet" [iSetConstr, ixSetConstr]



instance (Indexable a b, Data a, Ord a, Default a) => Default (IxSet a) where
    defaultValue = ISet []

instance (Ord a,Show a) => Show (IxSet a) where show = show . toSet

instance (Ord a,Read a,Data a,Indexable a b) => Read (IxSet a) where
    readsPrec n = mapFst fromSet . readsPrec n


class (Data b) => Indexable a b | a->b where
    empty :: IxSet a -- defines what types are indexed and queryable
    calcs :: a->b -- adds indexable values not found in the type
           -- if you don't want calculated values use DB a ()
           --should this be a fromDyn so we can provide a default impl?
           
noCalcs :: t -> ()
noCalcs _ = ()

inferIxSet :: String -> TH.Name -> TH.Name -> [TH.Name] -> Q [Dec]
inferIxSet ixset typeName calName entryPoints
    = do calInfo <- reify calName
         typeInfo <- reify typeName
         let (context,names) = case typeInfo of
                                 TyConI (DataD ctxt _ nms _ _) -> (ctxt,nms)
                                 TyConI (NewtypeD ctxt _ nms _ _) -> (ctxt,nms)
                                 TyConI (TySynD _ nms _) -> ([],nms)
                                 _ -> error "unexpected match"
             typeCon = foldl appT (conT typeName) (map varT names)
         case calInfo of
           VarI _ t _ _ ->
               let calType = getCalType t
                   getCalType (ForallT _names _ t') = getCalType t'
                   getCalType (AppT (AppT ArrowT _) t') = t'
                   getCalType t' = error ("Unexpected type: " ++ pprint t')
                   mkEntryPoint n = appE (conE 'Ix) (sigE (varE 'Map.empty) (forallT names (return context) $
                                                                             appT (appT (conT ''Map) (conT n)) (appT (conT ''Set) typeCon)))
               in do i <- instanceD' (return context) (appT (appT (conT ''Indexable) typeCon) (return calType))
                          [d| empty :: IxSet a
                              empty = IxSet $(listE (map mkEntryPoint entryPoints))
                              calcs :: a -> b
                              calcs = $(varE calName) |]
                     let ixType = appT (conT ''IxSet) typeCon
                     ixType' <- tySynD (mkName ixset) names ixType
                     return $ [i, ixType']  -- ++ d
           _ -> error "unexpected match"

-- modification operations

flatten :: (Typeable a, Data a) => a -> [Dynamic]
flatten x = case cast x of
                Just y -> [toDyn (y :: String)]
                Nothing -> toDyn x : concat (gmapQ flatten x)

type IndexOp =
    forall k a. (Ord k,Ord a) => k -> a -> Map k (Set a) -> Map k (Set a)

change :: (Data a, Ord a,Data b,Indexable a b) =>
          IndexOp -> a -> IxSet a -> IxSet a
change op x (ISet as) = change op x $ fromList as
change op x (IxSet indices) =
    IxSet $ update indices $ flatten (x,calcs x)
    where
    update [] _ = []
    update _ [] = []
    update (Ix index:is) dyns = Ix index':update is dyns'
        where
        keyType = typeOf ((undefined :: Map key (Set a) -> key) index)
        (ds,dyns') = partition (\d->dynTypeRep d == keyType) dyns
                     -- partition handles out of order indexes
        ii dkey = op (fromJust $ fromDynamic dkey) x
        index' = foldr ii index ds -- handle multiple values
    update _ _ = error "unexpected match"

insert :: (Data a, Ord a,Data b,Indexable a b) => a -> IxSet a -> IxSet a
insert = change Ix.insert

delete :: (Data a, Ord a,Data b,Indexable a b) => a -> IxSet a -> IxSet a
delete = change Ix.delete

updateIx :: (Indexable a b, Ord a, Data a, Typeable k)
         => k -> a -> IxSet a -> IxSet a
updateIx i new ixset = insert new $
                     maybe ixset (flip delete ixset) $
                     getOne $ ixset @= i

-- conversion operations

-- | Converts an IxSet to a Set of its elements
toSet :: Ord a => IxSet a -> Set a
toSet (IxSet (Ix ix:_)) = Map.fold Set.union Set.empty ix
toSet (IxSet []) = Set.empty
toSet (ISet lst) = Set.fromList lst
toSet _ = error "unexpected match"

toSet' :: Ord a => [Ix a] -> Set a
toSet' (Ix ix:_) = Map.fold Set.union Set.empty ix
toSet' [] = Set.empty
toSet' _ = error "unexpected match"

fromSet :: (Indexable a b, Ord a, Data a) => Set a -> IxSet a
fromSet set = Set.fold insert empty set

fromSet' :: (Indexable a b, Ord a, Data a) => Set a -> IxSet a
fromSet' set = Set.fold insert empty set

fromList :: (Indexable a b, Ord a, Data a) => [a] -> IxSet a
fromList = fromSet . Set.fromList

size :: Ord a => IxSet a -> Int
size = Set.size . toSet

-- | Converts an IxSet to its list of elements.
toList :: Ord a => IxSet a -> [a]
toList = Set.toList . toSet

toList' :: Ord a => [Ix a] -> [a]
toList' = Set.toList . toSet'

getOne :: Ord a => IxSet a -> Maybe a
getOne ixset = case toList ixset of
                   [x] -> Just x
                   _   -> Nothing

getOneOr :: Ord a => a -> IxSet a -> a
getOneOr def = fromMaybe def . getOne

-- set operations
(&&&) :: (Ord a, Data a, Indexable a b) => IxSet a -> IxSet a -> IxSet a
(&&&) = intersection

(|||) :: (Ord a, Data a, Indexable a b) => IxSet a -> IxSet a -> IxSet a
(|||) = union

infixr 5 &&&
infixr 5 |||

union :: (Ord a, Data a, Indexable a b) => IxSet a -> IxSet a -> IxSet a
union x1 x2 = fromSet $ Set.union (toSet x1) (toSet x2)

intersection :: (Ord a, Data a, Indexable a b) => IxSet a -> IxSet a -> IxSet a
intersection x1 x2 = fromSet $ Set.intersection (toSet x1) (toSet x2)


-- query operators
(@=), (@<), (@>), (@<=), (@>=)
    :: (Indexable a b, Data a, Ord a, Typeable k)
    => IxSet a -> k -> IxSet a
ix @= v = getEQ v ix
ix @< v = getLT v ix
ix @> v = getGT v ix
ix @<= v = getLTE v ix
ix @>= v = getGTE v ix

(@><), (@>=<), (@><=), (@>=<=)
    :: (Indexable a b, Data a, Ord a, Typeable k)
    => IxSet a -> (k, k) -> IxSet a
ix @>< (v1,v2) = getLT v2 $ getGT v1 ix
ix @>=< (v1,v2) = getLT v2 $ getGTE v1 ix
ix @><= (v1,v2) = getLTE v2 $ getGT v1 ix
ix @>=<= (v1,v2) = getLTE v2 $ getGTE v1 ix

(@+), (@*)
    :: (Indexable a b, Data a, Ord a, Typeable k)
    => IxSet a -> [k] -> IxSet a
ix @+ list = foldr union empty        $ map (ix @=) list
ix @* list = foldr intersection empty $ map (ix @=) list

getEQ :: (Indexable a b, Data a, Ord a, Typeable k)
      => k -> IxSet a -> IxSet a
getEQ = getOrd EQ

getLT :: (Indexable a b, Data a, Ord a, Typeable k)
      => k -> IxSet a -> IxSet a
getLT = getOrd LT

getGT :: (Indexable a b, Data a, Ord a, Typeable k)
      => k -> IxSet a -> IxSet a
getGT = getOrd GT

getLTE :: (Indexable a b, Data a, Ord a, Typeable k)
       => k -> IxSet a -> IxSet a

getLTE v ix = let ix2 = (getLT v ix) in union ix2 $ getEQ v ix

getGTE :: (Indexable a b, Data a, Ord a, Typeable k)
       => k -> IxSet a -> IxSet a
getGTE v ix = let ix2 = (getOrd GT v ix) in union ix2 $ getEQ v ix


getRange :: (Indexable a b, Typeable k, Ord a, Data a)
         => k -> k -> IxSet a -> IxSet a
getRange k1 k2 ixset = intersection (getGTE k1 ixset) (getLT k2 ixset)

groupBy::(Typeable k,Typeable t) =>  IxSet t -> [(k, [t])]
groupBy (IxSet indices) = collect indices
    where
    collect [] = []
    collect (Ix index:is) = maybe (collect is) f (fromDynamic $ toDyn index)
    collect _ = error "unexpected match"
    f = mapSnd Set.toList . Map.toList
groupBy _ = error "unexpected match"



rGroupBy :: (Typeable k, Typeable t) => IxSet t -> [(k, [t])]
rGroupBy = reverse . groupBy
    
--query impl function
getOrd :: (Indexable a b, Ord a, Data a, Typeable k)
       => Ordering -> k -> IxSet a -> IxSet a
getOrd ord v (IxSet indices) = collect indices
    where
    v' = toDyn v
    collect [] = empty
    collect (Ix index:is) = maybe (collect is) f $ fromDynamic v'
        where
        f v'' = foldr insert empty $
              case ord of
              LT -> lt
              GT -> gt
              EQ -> eq
            where
            (lt',eq',gt')=Map.splitLookup v'' index
            lt = concatMap (Set.toList . snd) $ Map.toList lt'
            gt = concatMap (Set.toList . snd) $ Map.toList gt'
            eq = maybe [] Set.toList eq'
    collect _ = error "unexpected match"
getOrd _ _ _ = error "unexpected match"

--we want a gGets that returns a list of all matches

{--
Optimization todo:

* can we avoid rebuilding the collection every time we query?
  does laziness take care of everything?

* nicer operators?

* good way to enforce that you don't query on the wrong type?

* nice way to do updates that doesn't involve reinserting the entire data

* can we index on xpath rather than just type?

--}

instance (Show a,Indexable a b,Data a,Ord a) => Monoid (IxSet a) where
    mempty=empty
    mappend = union

