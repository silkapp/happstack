{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module HSP.Identity 
    ( Ident
    , evalIdentity
    ) where

import HSP
import Control.Monad.Identity (Identity(Identity, runIdentity))
import qualified HSX.XMLGenerator as HSX

instance HSX.XMLGenerator Identity

instance HSX.XMLGen Identity where
    type HSX.XML Identity = XML
    newtype HSX.Child Identity = IChild { unIChild :: XML }
    newtype HSX.Attribute Identity = IAttr { unIAttr :: Attribute }
    genElement n attrs children = HSX.XMLGenT $ Identity (Element
                                                          (toName n)
                                                          (map unIAttr $ concatMap runIdentity $ map HSX.unXMLGenT attrs)
                                                          (map unIChild $ concatMap runIdentity $ map HSX.unXMLGenT children)
                                                         )
    xmlToChild = IChild

instance HSX.EmbedAsAttr Identity Attribute where
    asAttr = return . (:[]) . IAttr 

instance HSX.EmbedAsAttr Identity (Attr String Char) where
    asAttr (n := c)  = asAttr (n := [c])

instance HSX.EmbedAsAttr Identity (Attr String String) where
    asAttr (n := str)  = asAttr $ MkAttr (toName n, pAttrVal str)

instance HSX.EmbedAsAttr Identity (Attr String Bool) where
    asAttr (n := True)  = asAttr $ MkAttr (toName n, pAttrVal "true")
    asAttr (n := False) = asAttr $ MkAttr (toName n, pAttrVal "false")

instance HSX.EmbedAsAttr Identity (Attr String Int) where
    asAttr (n := i)  = asAttr $ MkAttr (toName n, pAttrVal (show i))


instance EmbedAsChild Identity Char where
    asChild = XMLGenT . Identity . (:[]) . IChild . pcdata . (:[])

instance EmbedAsChild Identity String where
    asChild = XMLGenT . Identity . (:[]) . IChild . pcdata


instance EmbedAsChild Identity XML where
    asChild = XMLGenT . Identity . (:[]) . IChild

instance EmbedAsChild Identity () where
  asChild () = return []

instance AppendChild Identity XML where
 appAll xml children = do
        chs <- children
        case xml of
         CDATA _ _       -> return xml
         Element n as cs -> return $ Element n as (cs ++ (map stripChild chs))

stripAttr :: HSX.Attribute Identity -> Attribute
stripAttr  (IAttr a) = a

stripChild :: HSX.Child Identity -> XML
stripChild (IChild c) = c

instance SetAttr Identity XML where
 setAll xml hats = do
        attrs <- hats
        case xml of
         CDATA _ _       -> return xml
         Element n as cs -> return $ Element n (foldr insert as (map stripAttr attrs)) cs

insert :: Attribute -> Attributes -> Attributes
insert = (:)

evalIdentity :: XMLGenT Identity XML -> XML
evalIdentity = runIdentity . HSX.unXMLGenT

type Ident = XMLGenT Identity
