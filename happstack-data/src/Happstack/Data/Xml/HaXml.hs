{-# LANGUAGE TemplateHaskell, FlexibleInstances,
             OverlappingInstances, UndecidableInstances #-}

module Happstack.Data.Xml.HaXml where

import Data.List
import Happstack.Data.Xml.Base
import qualified Text.XML.HaXml.Types as H

-- | 
isAttr :: Element -> Bool
isAttr (Attr {}) = True
isAttr _ = False

-- | Lifts toHaXml to act on lists of Elements 
toHaXmls :: [Element] -> [H.Content]
toHaXmls = map toHaXml

-- | Converts the Element to HaXml Element if the Element is of the form
-- Elem _ _ 
toHaXmlEl :: Element -> H.Element
toHaXmlEl el = let H.CElem el' = toHaXml el in el'

-- | Conversion function between the Happstack Element and HaXml Content types
toHaXml :: Element -> H.Content
toHaXml (Elem n es) = case partition isAttr es of
                      (as, xs) ->
                          H.CElem (H.Elem n (map toAttribute as) (toHaXmls xs))
toHaXml (CData x) = H.CString True x
-- We can't do better than wrap an attribute up in a fake element.
-- This shouldn't be happening in the real world anyway.
toHaXml a@(Attr {}) = toHaXml (Elem "JustAnAttr" [a])

-- | Converts an Element that is an Attr into a HaXml Attribute.  Will throw
-- an error if provided the wrong constructor.
toAttribute :: Element -> H.Attribute
toAttribute (Attr k v) = (k, H.AttValue [Left v])
toAttribute _ = error "toAttribute: Can't happen"

-- Is this function really necessary?
-- | Lifts fromHaXml to operate on lists
fromHaXmls :: [H.Content] -> [Element]
fromHaXmls = map fromHaXml

-- | Converts a HaXml Content to an Element
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
fromHaXml (H.CRef (H.RefChar x)) = 
    error $ "fromHaXml: Not implemented ref:" ++ (show x)
fromHaXml (H.CMisc (H.Comment _)) = CData ""
fromHaXml (H.CMisc (H.PI (_,_))) = CData ""

-- | Lifts fromAttribute to act on lists
fromAttributes :: [H.Attribute] -> [Element]
fromAttributes = map fromAttribute

-- | Will convert a HaXml attribute that is of the form [Left _] to a
-- Happstack Element.  Otherwise, will throw an error.
fromAttribute :: H.Attribute -> Element
fromAttribute (k, H.AttValue [Left v]) = Attr k v
fromAttribute _ = error "fromAttribute: Not implemented"
