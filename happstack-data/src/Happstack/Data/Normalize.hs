{-# LANGUAGE OverlappingInstances, UndecidableInstances,
             FlexibleContexts, FlexibleInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Happstack.Data.Normalize
-- Copyright   :  (c) Happstack.com 2009; (c) HAppS LLC 2007
-- License     :  BSD3
--
-- Maintainer  :  happs@googlegroups.com
-- Stability   :  experimental
-- Portability :  Not portable
--
-- Normalizing Haskell values.
--
-----------------------------------------------------------------------------

module Happstack.Data.Normalize
    (
        -- * The interface
        Normalize(normalize, normalizeRecursively),

        -- * Writing your own instances
        defaultNormalize,
        defaultNormalizeRecursively,

        -- * Advanced usage
        NormalizeD(..),
        normalizeProxy,
    )
    where

import Data.Generics.SYB.WithClass.Basics

-- | The 'Normalize' class provides a 'normalize' function, which
-- is intended to normalize values only at the top-level constructor,
-- and a 'normalizeRecursively' function, which is intended to
-- normalize all the subvalues and then normalize the top-level
-- constructor.
--
-- There is a default instance that matches all types, where 'normalize'
-- is 'id' and 'normalizeRecursively' applies 'normalizeRecursively' to
-- all of its children and then 'normalize' to the result.
--
-- If you want to actually do some normalization for a certain type,
-- then just define an instance for that type; this will take precedence
-- over the default instance.
class Data NormalizeD a => Normalize a where
    normalize :: a -> a
    normalize = defaultNormalize
    normalizeRecursively :: a -> a
    normalizeRecursively = defaultNormalizeRecursively

-- | This is the 'normalize' function in the default 'Normalize'
-- instance. It may be a useful building block when writing your own
-- instances.
defaultNormalize :: Normalize a => a -> a
defaultNormalize x = x

-- | This is the 'normalizeRecursively' function in the default
-- 'Normalize' instance. It may be a useful building block when writing
-- your own instances.
defaultNormalizeRecursively :: Normalize a => a -> a
defaultNormalizeRecursively = normalize . gmapT normalizeProxy (normalizeRecursivelyD dict)

-- | When writing your own generic functions for 'Normalize' you may
-- need to access the class methods through this datatype rather than
-- directly.
data NormalizeD a = NormalizeD { normalizeD :: a -> a,
                                 normalizeRecursivelyD :: a -> a }

-- | When writing your own generic functions for 'Normalize' you may
-- need this, the proxy value.
normalizeProxy :: Proxy NormalizeD
normalizeProxy = error "normalizeProxy"

instance Normalize t => Sat (NormalizeD t) where
    dict = NormalizeD { normalizeD = normalize,
                        normalizeRecursivelyD = normalizeRecursively }

instance Data NormalizeD a => Normalize a

