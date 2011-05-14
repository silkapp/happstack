{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -F -pgmFtrhsx -fno-warn-orphans #-}
module Happstack.Server.HSP.JMacro where

import qualified Data.ByteString.Char8 as S
import Data.ByteString.Lazy.UTF8       as LB (fromString)
import Control.Monad.Trans (lift)
import Control.Monad.State (MonadState(get,put))
import Happstack.Server (ToMessage(..))
import HSP
import Language.Javascript.JMacro (JStat(..), jsToDoc, jsSaturate, renderJs)
import Text.PrettyPrint.HughesPJ

class IntegerSupply m where 
    nextInteger :: m Integer

nextInteger' :: (MonadState Integer m) => m Integer
nextInteger' =
    do i <- get
       put (succ i)
       return i

instance (XMLGenerator m, IntegerSupply m) => EmbedAsChild m JStat where
  asChild jstat = 
      do i <- lift nextInteger
         asChild $
           <script type="text/javascript">
             <% render $ jsToDoc $ jsSaturate (Just ('i' : show i)) jstat %>
           </script>

instance (EmbedAsAttr m Attribute, IntegerSupply m, IsName n) => EmbedAsAttr m (Attr n JStat) where
  asAttr (n := jstat) = 
      do i <- lift nextInteger
         asAttr $ MkAttr (toName n, pAttrVal $ renderStyle lineStyle $ jsToDoc $ jsSaturate (Just ('i' : show i)) jstat)
      where
        lineStyle = style { mode= OneLineMode }

    
instance ToMessage JStat where
    toContentType _  = S.pack "text/javascript; charset=UTF-8"
    toMessage     js = LB.fromString (show $ renderJs js)
