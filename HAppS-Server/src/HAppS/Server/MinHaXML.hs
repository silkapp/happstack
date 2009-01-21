{-# LANGUAGE TemplateHaskell, FlexibleInstances,
             OverlappingInstances, UndecidableInstances, TypeSynonymInstances #-}


  
module HAppS.Server.MinHaXML where
-- Copyright (C) 2005 HAppS.org. All Rights Reserved.

import Text.XML.HaXml.Types as Types
import Text.XML.HaXml.Escape
import Text.XML.HaXml.Pretty as Pretty
import Text.XML.HaXml.Verbatim as Verbatim
import HAppS.Util.Common
import Data.Maybe
import System.Time

import HAppS.Data.Xml as Xml
import HAppS.Data.Xml.HaXml

type StyleURL=String
data StyleSheet = NoStyle
                | CSS {styleURL::StyleURL} 
                | XSL {styleURL::StyleURL} deriving (Read,Show)
hasStyleURL NoStyle = False
hasStyleURL _ = True 
type Element = Types.Element

	

isCSS (CSS _)=True
isCSS _ = False
isXSL = not.isCSS

t=textElem
l=listElem
e=emptyElem
(</<)=l
(<>)=t



xmlElem f = \name attrs val -> xmlelem name attrs (f val)
	where 
	xmlelem name attrs = Types.Elem name (map (uncurry attr) attrs)
	attr name val= (name,AttValue [Left val])

textElem = xmlElem (return.CString True)
emptyElem = \n a->xmlElem id n a []
listElem = xmlElem $ map CElem

cdataElem = CString  False

--simpleDoc xsl root = show $ document $ 
--	     Document (simpleProlog xsl) [] $ xmlEscape stdXmlEscaper root
simpleDocOld xsl = show . document . 
                flip (Document (simpleProlog xsl) []) [] . xmlStdEscape

simpleDoc style elem = ("<?xml version='1.0' encoding='UTF-8' ?>\n"++
                      if hasStyleURL style then pi else "") ++
                     (verbatim $ xmlStdEscape elem)
    where typeText=if isCSS style then "text/css" else "text/xsl"
          pi= "<?xml-stylesheet type=\""++ typeText  ++ 
              "\" href=\""++styleURL style++"\" ?>\n"


simpleDoc' style elem = (if hasStyleURL style then pi else "") ++
                        (verbatim $ xmlStdEscape elem)
    where typeText=if isCSS style then "text/css" else "text/xsl"
          pi= "<?xml-stylesheet type=\""++ typeText  ++ 
              "\" href=\""++styleURL style++"\" ?>\n"



xmlEscaper=stdXmlEscaper
xmlStdEscape = xmlEscape stdXmlEscaper
verbim x =verbatim x

simpleProlog style = 
    Prolog 
    (Just (XMLDecl "1.0" 
	   (Just $ EncodingDecl "UTF-8") 
	   Nothing -- (Just True) -- standalone declaration
	  ))
    [] Nothing -- (Just docType)
           (if url=="" then [] else [pi])
	where
	pi = PI ("xml-stylesheet", "type=\""++typeText++"\" href=\""++url++"\"")
	typeText = if isCSS style then "text/css" else "text/xsl"
	url=if hasStyleURL style then styleURL style else ""

nonEmpty name val = if val=="" then Nothing
					else Just $ textElem name [] val

getRoot (Document _ _ root _) = root

--toXML .< "App" attrs ./>
--toXML .< "App" attrs .> []
data XML a = XML StyleSheet a
--class HasStyle x where getStyle::x->StyleSheet
class ToElement x where toElement::x->Types.Element
		
{--
instance (ToElement x) => ToElement (Maybe x) where 
    toElement = maybe (emptyElem "Nothing" []) 
                (\x->listElem "Just" [] [toElement x])
--}

instance (ToElement x) => ToElement (Maybe x) where 
    toElement = maybe (emptyElem "Nothing" []) toElement

instance ToElement String where toElement s = textElem "String" [] s
instance ToElement Types.Element where toElement = id
instance ToElement CalendarTime where 
    toElement = recToEl "CalendarTime" 
                [attrFS "year" ctYear
                ,attrFS "month" (fromEnum.ctMonth)
                ,attrFS "day" ctDay
                ,attrFS "hour" ctHour
                ,attrFS "min" ctMin
                ,attrFS "sec" ctSec
                ,attrFS "time" time 
                ] []
        where time ct = epochPico ct

instance ToElement Int where toElement = toElement . show
instance ToElement Integer where toElement = toElement . show
instance ToElement Float where toElement = toElement . show
instance ToElement Double where toElement = toElement . show


instance (Xml a) => ToElement a where
    toElement = un . head . map toHaXml . toXml
        where
        un (CElem el) = el


--let TOD sec pico = toClockTime ct in sec*1000  
--class (Show x)=>ToAttr x where toAttr::x->String; toAttr=show



wrapElem tag x= listElem tag [] [toElement x]
elF tag f = wrapElem tag.f 
-- label !<=! field = wrapField label field
attrF name f rec = (name,quoteEsc $ f rec)
attrFS name f rec = (name,quoteEsc $ show $ f rec)
attrFMb r name f = maybe ("","") (\x->(name,quoteEsc $ r x)) . f 

--attrFMb r name f list= maybe list ((attrF name (r . fromJust . f)):list)

--(\x->(name,quoteEsc $ r x)) . f 
--(name,quoteEsc $ show $ f rec)

quoteEsc [] = []
quoteEsc ('"':list) = "&quot;" ++ quoteEsc list
quoteEsc (x:xs) = x:quoteEsc xs

--hexToInt

--hexToAlphaNum [] = []
--hexToAlphaNum (x:xs) = 

--attrShowF name f rec = (name,show $ f rec)

--quotescape \\ and " \"

recToEl name attrs els rec = listElem name attrs' (revmap rec els)
    where
    attrs' = filter (\ (x,_)->not $ null x) (revmap rec attrs)
listToEl name attrs = listElem name attrs . map toElement 

--listToEl list = listElem "List" [] $ map toElement list


--instance ToElement NELL where toElement s = e "x" []

--data NELL = NELL

toAttrs x = map (\ (s,f)->(s, f x)) 

{--
toElement rules:
1. if the attr is an instance of toElement then it is a child.
2. if it named and is type string then it is shown that way.
3. if it named and has non-string type then use show on the value.
4. if the attributes are not named then use the type as the label and
   make the text child be a show of the object.
--}


newtype ElString = ElString {elString::String} deriving (Eq,Ord,Read,Show)


{--
store thing as XML or do multiple parse passes?

--}
{--
elstring
--}
