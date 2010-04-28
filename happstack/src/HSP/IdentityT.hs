{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, TypeFamilies, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans -F -pgmF trhsx #-}
module HSP.IdentityT 
    ( evalIdentityT
    , IdentT
    , IdentityT(..)
    ) where

import Control.Applicative  (Applicative((<*>), pure))
import Control.Monad        (MonadPlus)
import Control.Monad.Writer (MonadWriter)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State  (MonadState)
import Control.Monad.RWS    (MonadRWS)
import Control.Monad.Trans  (MonadTrans(lift), MonadIO(liftIO))
import HSP
import qualified HSX.XMLGenerator as HSX

-- * IdentityT Monad Transformer

newtype IdentityT m a = IdentityT { runIdentityT :: m a }
    deriving (Functor, Monad, MonadWriter w, MonadReader r, MonadState s, MonadRWS r w s, MonadIO, MonadPlus)

instance (Applicative f) => Applicative (IdentityT f) where
    pure  = IdentityT . pure     
    (IdentityT f) <*> (IdentityT a) = IdentityT (f <*> a)

instance MonadTrans IdentityT where
    lift = IdentityT

-- * HSX.XMLGenerator for IdentityT

instance (Monad m, Functor m) => HSX.XMLGenerator (IdentityT m)

instance (Functor m, Monad m) => HSX.XMLGen (IdentityT m) where
    type HSX.XML (IdentityT m) = XML
    newtype HSX.Child (IdentityT m) = IChild { unIChild :: XML }
    newtype HSX.Attribute (IdentityT m) = IAttr { unIAttr :: Attribute }
    genElement n attrs children = HSX.XMLGenT $ 
                                  do attrs'    <- HSX.unXMLGenT (fmap (map unIAttr . concat) (sequence attrs))
                                     children' <- HSX.unXMLGenT (fmap (map unIChild . concat) (sequence children))
                                     return (Element (toName n) attrs' children')
    xmlToChild = IChild
    pcdataToChild = HSX.xmlToChild . pcdata

instance (Monad m, Functor m) => HSX.EmbedAsAttr (IdentityT m) Attribute where
    asAttr = return . (:[]) . IAttr 

instance (Monad m, Functor m) => HSX.EmbedAsAttr (IdentityT m) (Attr String Char) where
    asAttr (n := c)  = asAttr (n := [c])

instance (Monad m, Functor m) => HSX.EmbedAsAttr (IdentityT m) (Attr String String) where
    asAttr (n := str)  = asAttr $ MkAttr (toName n, pAttrVal str)

instance (Monad m, Functor m) => HSX.EmbedAsAttr (IdentityT m) (Attr String Bool) where
    asAttr (n := True)  = asAttr $ MkAttr (toName n, pAttrVal "true")
    asAttr (n := False) = asAttr $ MkAttr (toName n, pAttrVal "false")

instance (Monad m, Functor m) => HSX.EmbedAsAttr (IdentityT m) (Attr String Int) where
    asAttr (n := i)  = asAttr $ MkAttr (toName n, pAttrVal (show i))

instance (Monad m, Functor m) => EmbedAsChild (IdentityT m) Char where
    asChild = XMLGenT . return . (:[]) . IChild . pcdata . (:[])

instance (Monad m, Functor m) => EmbedAsChild (IdentityT m) String where
    asChild = XMLGenT . return . (:[]) . IChild . pcdata

instance (Monad m, Functor m) => EmbedAsChild (IdentityT m) (IdentityT m String) where
    asChild c = 
        do c' <- lift c
           lift . return . (:[]) . IChild . pcdata $ c'

instance (Monad m, Functor m) => EmbedAsChild (IdentityT m) XML where
    asChild = XMLGenT . return . (:[]) . IChild

instance (Monad m, Functor m) => EmbedAsChild (IdentityT m) () where
  asChild () = return []

instance (Monad m, Functor m) => AppendChild (IdentityT m) XML where
 appAll xml children = do
        chs <- children
        case xml of
         CDATA _ _       -> return xml
         Element n as cs -> return $ Element n as (cs ++ (map stripChild chs))

stripAttr :: (Monad m, Functor m) => HSX.Attribute (IdentityT m) -> Attribute
stripAttr  (IAttr a) = a

stripChild :: (Monad m, Functor m) => HSX.Child (IdentityT m) -> XML
stripChild (IChild c) = c

instance (Monad m, Functor m) => SetAttr (IdentityT m) XML where
 setAll xml hats = do
        attrs <- hats
        case xml of
         CDATA _ _       -> return xml
         Element n as cs -> return $ Element n (foldr insert as (map stripAttr attrs)) cs

insert :: Attribute -> Attributes -> Attributes
insert = (:)

evalIdentityT :: (Functor m, Monad m) => XMLGenT (IdentityT m) XML -> m XML
evalIdentityT = runIdentityT . HSX.unXMLGenT

type IdentT m = XMLGenT (IdentityT m) XML
