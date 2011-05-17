-- |This modules provides support for using JMacro with Happstack.
--
-- It provides the instance,
--
-- > instance ToMessage JStat 
--
-- Which will serve a 'JStat' value as @text/javascript; charset=UTF-8@.
module Happstack.Server.JMacro where

import qualified Data.ByteString.Char8 as S
import Data.ByteString.Lazy.UTF8       as LB
import Happstack.Server                (ToMessage(..))
import Language.Javascript.JMacro      (JStat(..), renderJs)
    
instance ToMessage JStat where
    toContentType _  = S.pack "text/javascript; charset=UTF-8"
    toMessage     js = LB.fromString (show $ renderJs js)
