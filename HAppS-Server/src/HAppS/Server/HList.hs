{-# OPTIONS -fallow-undecidable-instances #-}
module HAppS.Server.HList
{-    (ToXml(..)
    ,recordToElement, recordToXmlString,
     FromMessageEx(..)
    )-} where

import HAppS.Server.HTTP
import HAppS.Server.MessageWrap
import HAppS.Server.MinHaXML
import HAppS.Util.Common
import HAppS.Server.SimpleHTTP2 (xml)
import Data.Binary
import HList
import Text.XML.HaXml.Types

-- Record -> XML String

recordToXmlString label (Record r) = concat ["<",label,">", recordToXmlString' r, "</", label, ">"]

class    RecordToXmlString l    where recordToXmlString' :: l -> String
instance RecordToXmlString HNil where recordToXmlString' HNil = ""

instance ( ShowLabel l, RecordToXmlString ir, RecordToXmlString r) => RecordToXmlString (HCons (F l (Record ir)) r) where
  recordToXmlString' (HCons f@(F (Record v)) r) =
     let label = showLabel (labelF f)
     in concat ["<",label,">", recordToXmlString' v, "</", label, ">", recordToXmlString' r]

instance (ShowLabel l, Show v, RecordToXmlString r) => RecordToXmlString (HCons (F l v) r) where
  recordToXmlString' (HCons f@(F v) r)
     = let label = showLabel (labelF f)
        in concat ["<",label,">", show v, "</", label, ">", recordToXmlString' r]





-- Record -> ToElement (HaXml)

data ToXml t = ToXml Name [t]

instance RecordToElement r => ToElement (ToXml (Record r)) where
    toElement (ToXml top rs) = Elem top [] $ concat [ recordToElement r | Record r <- rs ]

class (Show el) => JSON el where jrender::el->String;jrender=show
instance JSON String where jrender = id
instance JSON Int
instance JSON Float
instance JSON Double
instance JSON Bool

instance Read (Record a) 
instance (RecordToElement r) => ToMessage (ToXml (Record r)) where toMessageM = xml 


class    RecordToElement l    where recordToElement :: l -> [Content]
instance RecordToElement HNil where recordToElement HNil = []

instance (ShowLabel l, RecordToElement ir, RecordToElement r) => 
    RecordToElement (HCons (F l (Record ir)) r) where
  recordToElement (HCons f@(F (Record v)) r) =
      CElem (Elem (showLabel (labelF f)) [] (recordToElement v)) : recordToElement r

instance (ShowLabel l, JSON v, RecordToElement r) => 
    RecordToElement (HCons (F l v) r) where
  recordToElement (HCons f@(F v) r) =
      CElem (textElem (showLabel (labelF f)) [] (jrender v)) : recordToElement r


-- Query params -> Record

class FromMessageEx m t where fromMessageEx :: Request -> m t

instance Monad m => FromMessageEx m (Record HNil) where fromMessageEx _ = return emptyRecord

instance FromMessage (Record HNil) where fromMessage = return emptyRecord
instance (Read elt,
          Typeable label,
          FromMessage (Record rest),
          HRLabelSet (HCons (F (Proxy label) elt) rest)) => 
    FromMessage (Record (HCons (F (Proxy label) elt) rest)) where
          fromMessageM m = do
                           let label = proxy::Proxy label
                           val <- maybeM $ lookMbRead m (showLabel label)
                           rest <- fromMessageM m
                           return (label .=. val .*. rest)


instance (Monad m, FromMessageEx m (F (Proxy label) elt), FromMessageEx m (Record tail),
                HRLabelSet (HCons (F (Proxy label) elt) tail)) 
    => FromMessageEx m (Record (HCons (F (Proxy label) elt) tail)) where
        fromMessageEx r = do x  <- fromMessageEx r
                             xs <- fromMessageEx r
                             return (x .*. xs)

instance (Monad m, Typeable label, Read et) => FromMessageEx m (F (Proxy label) et) where
    fromMessageEx r = do
        let l = proxy :: Proxy label
        et <- lookMb readM r $ showLabel l
        return (l .=. et)


-- Binary serialization for HList

data HBinaryPut = HBinaryPut

instance Binary t => Apply HBinaryPut t Put where apply _ = put
instance Binary t => Binary (F (Proxy l) t) where
    put (F x) = put x
    get       = fmap (proxy .=.) get

instance (HMapOut HBinaryPut r Put, HBinaryGet (Record r)) => Binary (Record r) where
    put (Record r) = hMapM_ HBinaryPut r
    get = hBinaryGet

class HBinaryGet t where hBinaryGet :: Get t
instance HBinaryGet (Record HNil) where
    hBinaryGet = return emptyRecord
instance (Binary (F (Proxy l) elt), HBinaryGet (Record tail), HRLabelSet (HCons (F (Proxy l) elt) tail)) =>
    HBinaryGet (Record (HCons (F (Proxy l) elt) tail)) where
        hBinaryGet = do x <- get
                        r <- hBinaryGet
                        return (x .*. r)


