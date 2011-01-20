{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module HSP.Google.Analytics 
    ( UACCT(..)
    , analytics
    , addAnalytics
    ) where

import Data.Generics (Data, Typeable)
import HSP
import Prelude hiding (head)

newtype UACCT = UACCT String -- ^ The UACCT provided to you by Google
    deriving (Read, Show, Eq, Ord, Typeable, Data)

-- | create the google analytics script tags
-- NOTE: you must put the <% analytics yourUACCT %> immediately before the </body> tag
-- See also: addAnalytics
analytics :: (XMLGenerator m) => UACCT -> GenXMLList m
analytics (UACCT uacct) =
    do a <- <script type="text/javascript">
              var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");
              document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));
            </script>
       b <- <script type="text/javascript">
              var pageTracker = _gat._getTracker("<% uacct %>");
              pageTracker._initData();
              pageTracker._trackPageview();
            </script>
       return [a,b]

-- | automatically add the google analytics scipt tags immediately before the </body> element
-- NOTE: this function is not idepotent
addAnalytics :: ( AppendChild m XML
                , EmbedAsChild m XML
                , EmbedAsAttr m Attribute
                , XMLGenerator m) 
             => UACCT 
             -> XMLGenT m XML 
             -> GenXML m
addAnalytics uacct pg =
    do page <- pg
       a <- analytics uacct
       case page of
         <html hattrs><[ head, body ]></html> ->
             <html hattrs>
                <% head %>
                <% body <: a %>
             </html>
         o -> error ("Failed to add analytics." ++ show o)

{- Example Analytics Code from Google:

<script type="text/javascript">
var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");
document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));
</script>
<script type="text/javascript">
var pageTracker = _gat._getTracker("UA-4353757-1");
pageTracker._initData();
pageTracker._trackPageview();
</script>
-}
