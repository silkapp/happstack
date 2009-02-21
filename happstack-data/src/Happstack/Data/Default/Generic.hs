
{-# LANGUAGE OverlappingInstances, UndecidableInstances #-}
{-# OPTIONS -fno-warn-orphans #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Happstack.Data.Default.Generic
-- Copyright   :  (c) 2009 Happstack.com; (c) 2007 HAppS LLC
-- License     :  BSD3
--
-- Maintainer  :  happs@googlegroups.com
-- Stability   :  experimental
-- Portability :  Not portable
--
-- Provides a 'Default' instance for all types. 'defaultDefaultValue' is
-- used for 'defaultValue'.
--
-----------------------------------------------------------------------------

module Happstack.Data.Default.Generic () where

import Happstack.Data.Default
import Data.Generics.SYB.WithClass.Basics
import Data.Generics.SYB.WithClass.Instances ()

-- This is an orphaned instanced.  This means the existance of this file causes
-- ghc to visit its .hi every time any files that depend on it are compiled,
-- just to see if this instance is need
instance Data DefaultD a => Default a

