{-# LANGUAGE TemplateHaskell, FlexibleInstances, CPP,
             OverlappingInstances, UndecidableInstances,
             DeriveDataTypeable, MultiParamTypeClasses #-}

module HAppS.Data.Xml.PrintParse where

import Control.Monad
import Text.PrettyPrint.HughesPJ
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Pretty
import Text.XML.HaXml.Types (Document(Document), Content(CElem))
import HAppS.Data.Xml.Base
import HAppS.Data.Xml.HaXml
import Data.Generics as G
import HAppS.Data.DeriveAll
import HAppS.Data.Default

#ifndef __HADDOCK__
$(deriveAll [''Read,''Show,''Default]
  [d|
      data W = W [K]
      data K = K String
   |]
 )
#endif


class ToString a where toString::a->String
instance ToString [Element] where
    toString = render . vcat . map (content . toHaXml)

instance ToString Element where
    toString = render . content . toHaXml

instance (Xml a,G.Data a) => ToString a where 
    toString a = toString $ toXml a


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
    fromString r x = do els <- fromString r x
                        fromXml r els

