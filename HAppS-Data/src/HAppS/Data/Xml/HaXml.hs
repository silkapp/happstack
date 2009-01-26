{-# LANGUAGE TemplateHaskell, FlexibleInstances,
             OverlappingInstances, UndecidableInstances #-}

module HAppS.Data.Xml.HaXml where

import Data.List
import HAppS.Data.Xml.Base
import qualified Text.XML.HaXml.Types as H

isAttr :: Element -> Bool
isAttr (Attr {}) = True
isAttr _ = False

toHaXmls :: [Element] -> [H.Content]
toHaXmls = map toHaXml

toHaXmlEl :: Element -> H.Element
toHaXmlEl el = let H.CElem el' = toHaXml el in el'


toHaXml :: Element -> H.Content
toHaXml (Elem n es) = case partition isAttr es of
                      (as, xs) ->
                          H.CElem (H.Elem n (map toAttribute as) (toHaXmls xs))
toHaXml (CData x) = H.CString True x
-- We can't do better than wrap an attribute up in a fake element.
-- This shouldn't be happening in the real world anyway.
toHaXml a@(Attr {}) = toHaXml (Elem "JustAnAttr" [a])

toAttribute :: Element -> H.Attribute
toAttribute (Attr k v) = (k, H.AttValue [Left v])
toAttribute _ = error "toAttribute: Can't happen"

fromHaXmls :: [H.Content] -> [Element]
fromHaXmls = map fromHaXml

fromHaXml :: H.Content -> Element
fromHaXml (H.CElem (H.Elem n as xs))
    = Elem n (fromAttributes as ++ fromHaXmls xs)
fromHaXml (H.CString _ x) = CData x
fromHaXml (H.CRef (H.RefEntity "amp")) = CData "&"
fromHaXml (H.CRef (H.RefEntity "lt")) = CData "<"
fromHaXml (H.CRef (H.RefEntity "gt")) = CData ">"
fromHaXml (H.CRef (H.RefEntity "apos")) = CData "'"
fromHaXml (H.CRef (H.RefEntity "quot")) = CData "\""
fromHaXml (H.CRef (H.RefEntity x)) = 
    error $ "fromHaXml: Not implemented ref:" ++ x
fromHaXml (H.CMisc (H.Comment _)) = CData ""
fromHaXml (H.CMisc (H.PI (_,_))) = CData ""



fromAttributes :: [H.Attribute] -> [Element]
fromAttributes = map fromAttribute

fromAttribute :: H.Attribute -> Element
fromAttribute (k, H.AttValue [Left v]) = Attr k v
fromAttribute _ = error "fromAttribute: Not implemented"




