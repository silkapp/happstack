{-# LANGUAGE OverlappingInstances, UndecidableInstances,
             FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  HAppS.Data.Default
-- Copyright   :  (c) 2007 HAppS LLC
-- License     :  BSD3
--
-- Maintainer  :  AlexJacobson@HAppS.org
-- Stability   :  experimental
-- Portability :  Not portable
--
-- Provides default values for Haskell datatypes.
--
-----------------------------------------------------------------------------

module HAppS.Data.Default
    (
        -- * The interface
        Default(defaultValue),

        -- * Writing your own instances
        defaultDefaultValue,

        -- * Advanced usage
        DefaultD(..),
        defaultProxy,
    )
    where

import qualified Data.ByteString.Char8 as BSC
import Data.Generics.SYB.WithClass.Basics
import Data.Generics.SYB.WithClass.Instances ()
import Data.Int
import Data.Word
import qualified Data.Map as M
import qualified Data.Set as S
import Foreign.ForeignPtr

-- | The 'Default' class provides a 'defaultValue' value, which
-- is the default value for that type.
--
-- There is no instance for arbitrary types by default, but if you
-- declare an instance without providing the value then one will be
-- built using the first constructor. 'defaultValue' is used to provide
-- values for any arguments of the constructor.
--
-- If you want an instance for all types then import
-- "HAppS.Data.Default.Generic".
class (Data DefaultD a) => Default a where
    defaultValue :: a
    defaultValue = defaultDefaultValue

-- | This is the 'defaultValue' that is used in an instance if you don't
-- specify one. It may be a useful building block when writing your own
-- instances.
defaultDefaultValue :: (Data DefaultD a,Default a) => a
defaultDefaultValue = res
    where res = case datarep $ dataTypeOf defaultProxy res of
                    AlgRep (c:_) ->
                        fromConstrB defaultProxy (defaultValueD dict) c
                    r ->
                        error ("defaultDefaultValue: Bad DataRep: " ++ show r)

-- | When writing your own generic functions for 'Default' you may
-- need to access the class method through this datatype rather than
-- directly.
data DefaultD a = DefaultD { defaultValueD :: a }

-- | When writing your own generic functions for 'Default' you may
-- need this, the proxy value.
defaultProxy :: Proxy DefaultD
defaultProxy = error "defaultProxy"

instance Default t => Sat (DefaultD t) where
    dict = DefaultD { defaultValueD = defaultValue }

instance Default a => Default [a] where
    defaultValue = []

instance Default Int     where defaultValue = 0
instance Default Int8    where defaultValue = 0
instance Default Int16   where defaultValue = 0
instance Default Int32   where defaultValue = 0
instance Default Int64   where defaultValue = 0
instance Default Word    where defaultValue = 0
instance Default Word8   where defaultValue = 0
instance Default Word16  where defaultValue = 0
instance Default Word32  where defaultValue = 0
instance Default Word64  where defaultValue = 0
instance Default Integer where defaultValue = 0
instance Default Float   where defaultValue = 0
instance Default Double  where defaultValue = 0

instance (Default a, Default b) => Default (Either a b) where
    defaultValue = Left defaultValue

instance Default () where
    defaultValue = ()
instance (Default a, Default b) => Default (a,b) where
    defaultValue = (defaultValue, defaultValue)
instance (Default a, Default b, Default c) => Default (a,b,c) where
    defaultValue = (defaultValue, defaultValue, defaultValue)
instance (Default a, Default b, Default c, Default d) => Default (a,b,c,d) where
    defaultValue = (defaultValue, defaultValue, defaultValue, defaultValue)


instance Default Char where
    defaultValue = 'A'

instance Default a => Default (Maybe a) where
    defaultValue = Nothing

instance Default BSC.ByteString where
    defaultValue = BSC.pack ""

-- We don't really want this instance, but we need it for the ByteString
-- instance
instance Default a => Default (ForeignPtr a) where
    defaultValue = error "defaultValue: ForeignPtr"

instance (Data DefaultD a, Data DefaultD b, Ord a) => Default (M.Map a b) 
instance (Data DefaultD a, Ord a) => Default (S.Set a)