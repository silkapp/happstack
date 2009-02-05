
{-# LANGUAGE OverlappingInstances, UndecidableInstances #-}

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

instance Data DefaultD a => Default a

