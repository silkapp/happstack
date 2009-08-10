{-# LANGUAGE UndecidableInstances, OverlappingInstances, ScopedTypeVariables, GADTs, PatternSignatures,
    GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Happstack.Data.Serialize
    ( Serialize(..), Version(..), Migrate(..), Mode(..), Contained, contain, extension,
      safeGet, safePut, getSafeGet, getSafePut, serialize, deserialize, collectVersions,
      Object(objectType), mkObject, deserializeObject, parseObject,
      module Happstack.Data.Proxy
    ) where

import Control.Monad.Identity
import Data.Int()
import Foreign
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as B

import Happstack.Data.Migrate
import Happstack.Data.Proxy

import Data.Typeable
import qualified Data.Map as M
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set

import Data.Binary     as B
import Data.Binary.Put as B
import Data.Binary.Get as B

--------------------------------------------------------------
-- Core types
--------------------------------------------------------------

data Contained a = Contained {unsafeUnPack :: a}

-- | Lifts the provided value into Contained
contain :: a -> Contained a
contain = Contained

data Previous a = forall b. (Serialize b, Migrate b a) => Previous (Proxy b)

mkPrevious :: forall a b. (Serialize b, Migrate b a) => Proxy b -> Previous a
mkPrevious Proxy = Previous (Proxy :: Proxy b)

-- | Creates a Mode that is a new version of the type carried by the provided proxy
-- and with the provided version number.  Note that since VersionId is an instance of
-- Num that you may use int literals when calling extension, e.g. 
-- @extension 1 (Proxy :: Proxy OldState)@
extension :: forall a b. (Serialize b, Migrate b a) => VersionId a -> Proxy b -> Mode a
extension vs prox = Versioned vs (Just (mkPrevious prox))

newtype VersionId a = VersionId {unVersion :: Int} deriving (Num,Read,Show,Eq)
instance Binary (VersionId a) where
    get = liftM VersionId get
    put = put . unVersion


data Mode a = Primitive -- ^ Data layout won't change. Used for types like Int and Char.
            | Versioned (VersionId a) (Maybe (Previous a))

-- | The Version type class is used to describe whether a type is fundamental
-- or if it is meant to extend another type.  For a user defined type that
-- does not extend any others, one can use the default instance of Version, e.g.
-- @instance Version MyType@ to define it has having a version id of 0 and previous
-- type.
class Version a where
    mode :: Mode a
    mode = Versioned 0 Nothing

class (Typeable a, Version a) => Serialize a where
    getCopy :: Contained (Get a)
    putCopy :: a -> Contained Put

--------------------------------------------------------------
-- Implementation
--------------------------------------------------------------

getSafeGet :: forall a. Serialize a => Get (Get a)
getSafeGet = case mode :: Mode a of
               Primitive -> return (unsafeUnPack getCopy)
               Versioned wantedVersion mbPrevious
                         -> do storedVersion <- get
                               return (safeGetVersioned wantedVersion mbPrevious storedVersion)

getSafePut :: forall a. Serialize a => PutM (a -> Put)
getSafePut = case mode :: Mode a of
               Primitive -> return (unsafeUnPack . putCopy)
               Versioned vs _
                         -> do B.put vs
                               return (unsafeUnPack . putCopy)


-- | Equivalent of Data.Binary.put for instances of Serialize.  
-- Takes into account versioning of types.
safePut :: forall a. Serialize a => a -> Put
safePut val = do fn <- getSafePut
                 fn val

-- | Equivalent of Data.Binary.get for instances of Serialize
-- Takes into account versioning of types.
safeGet :: forall a. Serialize a => Get a
safeGet = join getSafeGet

safeGetVersioned :: forall a b. (Serialize b) => VersionId b -> Maybe (Previous b) -> VersionId a -> B.Get b
safeGetVersioned wantedVersion mbPrevious storedVersion
    = case compareVersions storedVersion wantedVersion of
        GT -> error $ "Version tag too large: " ++ show (wantedVersion,storedVersion) ++ " (" ++ tStr ++ ")"
        EQ -> unsafeUnPack getCopy
        LT -> case mbPrevious of
                Nothing -> error $ "No previous version (" ++ tStr ++ ")"
                Just (Previous (_ :: Proxy f) :: Previous b)
                    -> case mode of
                         Primitive -> error $ "Previous version marked as a Primitive (" ++ tStr ++ ")"
                         Versioned wantedVersion' mbPrevious'
                             -> do old <- safeGetVersioned wantedVersion' mbPrevious' storedVersion :: B.Get f
                                   return $ migrate old
    where tStr = show (typeOf (error "huh?" :: b))

-- | Compares the numeric value of the versions
compareVersions :: VersionId a -> VersionId b -> Ordering
compareVersions v1 v2 = compare (unVersion v1) (unVersion v2)

-- | Pure version of 'safePut'.  Serializes to a ByteString
serialize :: Serialize a => a -> L.ByteString
serialize = runPut . safePut

-- | Pure version of 'safeGet'.  Parses a ByteString into the expected type
-- and a remainder.
deserialize :: Serialize a => L.ByteString -> (a, L.ByteString)
deserialize bs = case runGetState safeGet bs 0 of
                   (val, rest, _offset) -> (val, rest)

-- | Version lookups
collectVersions :: forall a . (Typeable a, Version a) => Proxy a -> [L.ByteString]
collectVersions prox
    = case mode :: Mode a of
        Primitive                          -> [thisType]
        Versioned _ Nothing                -> [thisType]
        Versioned _ (Just (Previous prev)) -> thisType : (collectVersions prev)
    where thisType = (L.pack . show . typeOf . unProxy) prox

--------------------------------------------------------------
-- Instances
--------------------------------------------------------------

instance Version Int where mode = Primitive
instance Serialize Int where
    getCopy = contain get; putCopy = contain . put
instance Version Integer where mode = Primitive
instance Serialize Integer where
    getCopy = contain get; putCopy = contain . put
instance Version Float where mode = Primitive
instance Serialize Float where
    getCopy = contain get; putCopy = contain . put
instance Version Double where mode = Primitive
instance Serialize Double where
    getCopy = contain get; putCopy = contain . put
instance Version L.ByteString where mode = Primitive
instance Serialize L.ByteString where
    getCopy = contain get; putCopy = contain . put
instance Version B.ByteString where mode = Primitive
instance Serialize B.ByteString where
    getCopy = contain get; putCopy = contain . put
instance Version Char where mode = Primitive
instance Serialize Char where
    getCopy = contain get; putCopy = contain . put
instance Version Word8 where mode = Primitive
instance Serialize Word8 where
    getCopy = contain get; putCopy = contain . put
instance Version Word16 where mode = Primitive
instance Serialize Word16 where
    getCopy = contain get; putCopy = contain . put
instance Version Word32 where mode = Primitive
instance Serialize Word32 where
    getCopy = contain get; putCopy = contain . put
instance Version Word64 where mode = Primitive
instance Serialize Word64 where
    getCopy = contain get; putCopy = contain . put
instance Version Ordering where mode = Primitive
instance Serialize Ordering where
    getCopy = contain get; putCopy = contain . put
instance Version Int8 where mode = Primitive
instance Serialize Int8 where
    getCopy = contain get; putCopy = contain . put
instance Version Int16 where mode = Primitive
instance Serialize Int16 where
    getCopy = contain get; putCopy = contain . put
instance Version Int32 where mode = Primitive
instance Serialize Int32 where
    getCopy = contain get; putCopy = contain . put
instance Version Int64 where mode = Primitive
instance Serialize Int64 where
    getCopy = contain get; putCopy = contain . put
instance Version () where mode = Primitive
instance Serialize () where
    getCopy = contain get; putCopy = contain . put
instance Version Bool where mode = Primitive
instance Serialize Bool where
    getCopy = contain get; putCopy = contain . put
instance Version (Either a b) where mode = Primitive
instance (Serialize a, Serialize b) => Serialize (Either a b) where
    getCopy = contain $ do n <- get
                           if n then liftM Right safeGet
                                else liftM Left safeGet
    putCopy (Right a) = contain $ put True >> safePut a
    putCopy (Left a) = contain $ put False >> safePut a
instance Version (a,b) where mode = Primitive
instance (Serialize a, Serialize b) => Serialize (a,b) where
    getCopy = contain $ liftM2 (,) safeGet safeGet
    putCopy (a,b) = contain $ safePut a >> safePut b
instance Version (a,b,c) where mode = Primitive
instance (Serialize a, Serialize b, Serialize c) => Serialize (a,b,c) where
    getCopy = contain $ liftM3 (,,) safeGet safeGet safeGet
    putCopy (a,b,c) = contain $ safePut a >> safePut (b,c)
instance Version (a,b,c,d) where mode = Primitive
instance (Serialize a, Serialize b, Serialize c, Serialize d) => Serialize (a,b,c,d) where
    getCopy = contain $ liftM4 (,,,) safeGet safeGet safeGet safeGet
    putCopy (a,b,c,d) = contain $ safePut a >> safePut (b,c,d)
instance Version (a,b,c,d,e) where mode = Primitive
instance (Serialize a, Serialize b, Serialize c, Serialize d, Serialize e) => Serialize (a,b,c,d,e) where
    getCopy = contain $ liftM5 (,,,,) safeGet safeGet safeGet safeGet safeGet
    putCopy (a,b,c,d,e) = contain $ safePut a >> safePut (b,c,d,e)

instance Version (Proxy a) where mode = Primitive
instance Typeable a => Serialize (Proxy a) where
    getCopy = contain $ return Proxy
    putCopy Proxy = contain $ return ()

instance Version [a] where mode = Primitive
instance Serialize a => Serialize [a] where
    getCopy = contain $
              do n <- get
                 getSafeGet >>= replicateM n
    putCopy lst
        = contain $
          do put (length lst)
             getSafePut >>= forM_ lst

instance Version (Maybe a) where mode = Primitive
instance Serialize a => Serialize (Maybe a) where
    getCopy = contain $ do n <- get
                           if n then liftM Just safeGet
                                else return Nothing
    putCopy (Just a) = contain $ put True >> safePut a
    putCopy Nothing = contain $ put False

instance Version (Set.Set a) where mode = Primitive
instance (Serialize a, Ord a) => Serialize (Set.Set a) where
    getCopy = contain $ fmap Set.fromAscList safeGet
    putCopy = contain . safePut . Set.toList

instance Version (Map.Map a b) where mode = Primitive
instance (Serialize a,Serialize b, Ord a) => Serialize (Map.Map a b) where
    getCopy = contain $ fmap Map.fromAscList safeGet
    putCopy = contain . safePut . Map.toList

instance Version (IntMap.IntMap a) where mode = Primitive
instance (Serialize a) => Serialize (IntMap.IntMap a) where
    getCopy = contain $ fmap IntMap.fromAscList safeGet
    putCopy = contain . safePut . IntMap.toList


--------------------------------------------------------------
-- Object serialization
--------------------------------------------------------------


-- | 'deserialize' specialized to Objects 
deserializeObject :: L.ByteString -> (Object, L.ByteString)
deserializeObject = deserialize

-- | Attempts to convert an Object back into its base type.
-- If the conversion fails 'error' will be called.
parseObject :: Serialize a => Object -> a
parseObject (Object objType objData)
    = let res = runGet safeGet objData
          resType = show (typeOf res)
      in if objType /= resType
         then error $ "Failed to parse object of type '" ++ objType ++ "'. Expected type '" ++ resType ++ "'"
         else res

-- | Serializes data and stores it along with its type name in an Object
mkObject :: Serialize a => a -> Object
mkObject obj = Object { objectType = show (typeOf obj)
                      , objectData = serialize obj }

-- | Uniform container for any serialized data.  It contains a string rep of the type
-- and the actual data serialized to a byte string.
data Object = Object { objectType :: String
                     , objectData :: L.ByteString
                     }  deriving (Typeable,Show)

instance Version Object
instance Serialize Object where
    putCopy (Object objType objData) = contain $ put (objType, objData)
    getCopy = contain $
              do (objType, objData) <- get
                 return (Object objType objData)
