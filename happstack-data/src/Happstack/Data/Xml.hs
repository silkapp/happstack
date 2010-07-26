{-# LANGUAGE CPP #-}
module Happstack.Data.Xml (
    module Happstack.Data.Xml.Base,
#if defined (MIN_VERSION_haxml)
    module Happstack.Data.Xml.PrintParse
#endif
                          ) where

import Happstack.Data.Xml.Base
#if defined (MIN_VERSION_haxml)
#if MIN_VERSION_haxml(1,20,0)
import Happstack.Data.Xml.PrintParse
#else
import Happstack.Data.Xml.PrintParse13 as Happstack.Data.Xml.PrintParse
#endif
#endif
import Happstack.Data.Xml.Instances ()

