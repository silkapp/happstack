{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module HSP.WebT where

import HSP
import Control.Applicative ((<$>))
import qualified HSX.XMLGenerator as HSX
import Happstack.Server (WebT)

instance (Monad m) => HSX.XMLGen (WebT m) where
    type HSX.XML (WebT m) = XML
    newtype HSX.Child (WebT m) = WChild { unWChild :: XML }
    newtype HSX.Attribute (WebT m) = WAttr { unWAttr :: Attribute }
    genElement n attrs children = 
        do attribs <- map unWAttr <$> asAttr attrs
           childer <- flattenCDATA . map unWChild <$> asChild children
           HSX.XMLGenT $ return (Element
                              (toName n)
                              attribs
                              childer
                             )
    xmlToChild = WChild

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

instance (Monad m) => EmbedAsChild (WebT m) Char where
    asChild = XMLGenT . return . (:[]) . WChild . pcdata . (:[])

instance (Monad m) => EmbedAsChild (WebT m) String where
    asChild = XMLGenT . return . (:[]) . WChild . pcdata

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
