{-# LANGUAGE TemplateHaskell, FlexibleInstances, CPP,
             OverlappingInstances, UndecidableInstances,
             DeriveDataTypeable, MultiParamTypeClasses #-}

module Happstack.Data.Xml.PrintParse where

import Control.Monad
import Text.PrettyPrint.HughesPJ
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Pretty
import Text.XML.HaXml.Types (Document(Document), Content(CElem))
import Happstack.Data.Xml.Base
import Happstack.Data.Xml.HaXml
import Data.Generics as G
import Happstack.Data.DeriveAll
import Happstack.Data.Default

$(deriveAll [''Read,''Show,''Default]
  [d|
      data W = W [K]
      data K = K String
   |]
 )


class ToString a where toString::a->String
instance ToString [Element] where
    toString = render . vcat . map (content . toHaXml)

instance ToString Element where
    toString = render . content . toHaXml

instance (Xml a,G.Data a) => ToString a where 
    toString = toString . toXml


class FromString a where
    fromString :: Monad m => Rigidity m -> String -> m a

instance FromString Element where
    fromString _ s = case xmlParse "NoFile" s of
                       Document _ _ e _ ->
                           return $ fromHaXml $ CElem e
                       -- XXX Currently we assume this always succeeds,
                       -- but we should be allowing for the possibility of
                       -- failure
                       -- _ -> Nothing

instance FromString [Element] where
    fromString r s = liftM (: []) $ fromString r s

instance (Xml a,G.Data a) => FromString a where
    fromString r x = fromString r x >>= fromXml r

