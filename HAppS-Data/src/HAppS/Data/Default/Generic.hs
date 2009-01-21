
{-# LANGUAGE OverlappingInstances, UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  HAppS.Data.Default.Generic
-- Copyright   :  (c) 2007 HAppS LLC
-- License     :  BSD3
--
-- Maintainer  :  AlexJacobson@HAppS.org
-- Stability   :  experimental
-- Portability :  Not portable
--
-- Provides a 'Default' instance for all types. 'defaultDefaultValue' is
-- used for 'defaultValue'.
--
-----------------------------------------------------------------------------

module HAppS.Data.Default.Generic () where

import HAppS.Data.Default
import Data.Generics.SYB.WithClass.Basics
import Data.Generics.SYB.WithClass.Instances ()

instance Data DefaultD a => Default a

