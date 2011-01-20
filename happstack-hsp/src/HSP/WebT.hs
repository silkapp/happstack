{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module HSP.WebT where

import HSP
import Control.Monad (liftM)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified HSX.XMLGenerator as HSX
import Happstack.Server.Internal.Monads (WebT)

instance (Monad m) => HSX.XMLGen (WebT m) where
    type HSX.XML (WebT m) = XML
    newtype HSX.Child (WebT m) = WChild { unWChild :: XML }
    newtype HSX.Attribute (WebT m) = WAttr { unWAttr :: Attribute }
    genElement n attrs children = 
        do attribs <- map unWAttr `liftM` asAttr attrs
           childer <- (flattenCDATA . map unWChild) `liftM` asChild children
           HSX.XMLGenT $ return (Element
                              (toName n)
                              attribs
                              childer
                             )
    xmlToChild = WChild
    pcdataToChild = HSX.xmlToChild . pcdata

flattenCDATA :: [XML] -> [XML]
flattenCDATA cxml = 
                case flP cxml [] of
                 [] -> []
                 [CDATA _ ""] -> []
                 xs -> xs                       
    where
        flP :: [XML] -> [XML] -> [XML]
        flP [] bs = reverse bs
        flP [x] bs = reverse (x:bs)
        flP (x:y:xs) bs = case (x,y) of
                           (CDATA e1 s1, CDATA e2 s2) | e1 == e2 -> flP (CDATA e1 (s1++s2) : xs) bs
                           _ -> flP (y:xs) (x:bs)


instance (Monad m, Functor m) => IsAttrValue (WebT m) T.Text where
    toAttrValue = toAttrValue . T.unpack

instance (Monad m, Functor m) => IsAttrValue (WebT m) TL.Text where
    toAttrValue = toAttrValue . TL.unpack

instance (Monad m) => HSX.EmbedAsAttr (WebT m) Attribute where
    asAttr = return . (:[]) . WAttr 

instance (Monad m) => HSX.EmbedAsAttr (WebT m) (Attr String Char) where
    asAttr (n := c)  = asAttr (n := [c])

instance (Monad m) => HSX.EmbedAsAttr (WebT m) (Attr String String) where
    asAttr (n := str)  = asAttr $ MkAttr (toName n, pAttrVal str)

instance (Monad m) => HSX.EmbedAsAttr (WebT m) (Attr String Bool) where
    asAttr (n := True)  = asAttr $ MkAttr (toName n, pAttrVal "true")
    asAttr (n := False) = asAttr $ MkAttr (toName n, pAttrVal "false")

instance (Monad m) => HSX.EmbedAsAttr (WebT m) (Attr String Int) where
    asAttr (n := i)  = asAttr $ MkAttr (toName n, pAttrVal (show i))

instance (Monad m, Functor m, IsName n) => (EmbedAsAttr (WebT m) (Attr n TL.Text)) where
    asAttr (n := a) = asAttr $ MkAttr (toName n, pAttrVal $ TL.unpack a)

instance (Monad m, Functor m, IsName n) => (EmbedAsAttr (WebT m) (Attr n T.Text)) where
    asAttr (n := a) = asAttr $ MkAttr (toName n, pAttrVal $ T.unpack a)

instance (Monad m) => EmbedAsChild (WebT m) Char where
    asChild = XMLGenT . return . (:[]) . WChild . pcdata . (:[])

instance (Monad m) => EmbedAsChild (WebT m) String where
    asChild = XMLGenT . return . (:[]) . WChild . pcdata

instance (Monad m, Functor m) => (EmbedAsChild (WebT m) TL.Text) where
    asChild = asChild . TL.unpack

instance (Monad m, Functor m) => (EmbedAsChild (WebT m) T.Text) where
    asChild = asChild . T.unpack

instance (Monad m) => EmbedAsChild (WebT m) XML where
    asChild = XMLGenT . return . (:[]) . WChild

instance Monad m => EmbedAsChild (WebT m) () where
  asChild () = return []

instance (Monad m) => AppendChild (WebT m) XML where
 appAll xml children = do
        chs <- children
        case xml of
         CDATA _ _       -> return xml
         Element n as cs -> return $ Element n as (cs ++ (map unWChild chs))

instance (Monad m) => SetAttr (WebT m) XML where
 setAll xml hats = do
        attrs <- hats
        case xml of
         CDATA _ _       -> return xml
         Element n as cs -> return $ Element n (foldr (:) as (map unWAttr attrs)) cs

instance (Monad m) => XMLGenerator (WebT m)
