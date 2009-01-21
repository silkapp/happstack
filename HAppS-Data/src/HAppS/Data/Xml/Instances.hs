{-# LANGUAGE TemplateHaskell, FlexibleInstances,
             OverlappingInstances, UndecidableInstances, CPP,
             TypeSynonymInstances, PatternGuards,
             MultiParamTypeClasses #-}

module HAppS.Data.Xml.Instances where

import Data.Char
import Data.List
import HAppS.Data.Xml.Base
import Data.Generics.SYB.WithClass.Basics
import Data.Generics.SYB.WithClass.Instances ()
import Data.Maybe
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import HAppS.Data.Default
import HAppS.Util.Common



instance Xml Element where
    toXml = (:[])


-- The Xml [a] context is a bit scary, but if we don't have it then
-- GHC complains about overlapping instances
instance (Xml a, Xml [a]) => Xml [a] where
    toXml = concatMap toXml
    readXml r = f [] []
        where f acc_xs acc_vs [] = Just (reverse acc_xs, reverse acc_vs)
              f acc_xs acc_vs (x:xs) = case readXml r [x] of
                                           Just ([], v) ->
                                               f acc_xs (v:acc_vs) xs
                                           _ ->
                                               f (x:acc_xs) acc_vs xs


--data List'  a = Nil' | Cons a (List' a)






instance Xml Bool where
    toXml True = [CData "1"]
    toXml False = [CData "0"]
    readXml = readXmlWith f
        where f _ (CData "1") = Just True
              f _ (CData "0") = Just False
              f _ (CData "True") = Just True
              f _ (CData "False") = Just False
              f _ (CData "T") = Just True
              f _ (CData "F") = Just False
              f _ _ = Nothing

instance Default Bool where defaultValue= False

instance Xml String where
    toXml x = [CData x]
    readXml = readXmlWith f
        where f _ (CData x) = Just x
              f _ _ = Nothing

instance Xml Char where
    toXml x = [CData (x:[])]
    readXml = readXmlWith f
        where f _ (CData (x:[])) = Just x
              f _ _ = Nothing

instance Xml ByteString where
    toXml x = [CData $ BS.unpack x]
    readXml = readXmlWith f
        where f _ (CData x) = Just $ BS.pack x
              f _ _ = Nothing

instance Xml [String] where
    toXml xs = [CData $ concat $ intersperse "," xs]
    readXml = readXmlWith f
        where f _ (CData x) = Just $ words $ noCommas x

#ifndef __HADDOCK__
$( xmlShowCDatas [''Int, ''Integer, ''Float, ''Double] )
$( xmlCDataLists [''Int, ''Integer, ''Float, ''Double] )
#endif

instance Xml a => Xml (Maybe a) where
    toXml = transparentToXml
    -- We can't use transparentReadXml or Nothing would always win, as
    -- it is first in the list of constructors
    readXml r = aConstrFromElements r
              $ map (toConstr xmlProxy) [Just (), Nothing]

#ifndef __HADDOCK__
$( transparentXml ''Either )
$( transparentXml ''() )
$( transparentXml ''(,) )
$( transparentXml ''(,,) )
$( transparentXml ''(,,,) )
#endif

