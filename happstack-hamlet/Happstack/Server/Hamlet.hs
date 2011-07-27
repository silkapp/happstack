-- |This module provides support for using Hamlet with Happstack. Hamlet provides type-safe HTML generation. More information can be found at <http://patch-tag.com/r/mae/happstack/snapshot/current/content/pretty/happstack-hamlet/demo.hs> and <http://hackage.haskell.org/package/hamlet>.
module Happstack.Server.Hamlet 
    ( hamletToResponse 
    )
    where

import Data.Text         (Text)
import Text.Hamlet       (Hamlet)
import Happstack.Server  (Response, toResponse)

-- |turn a 'Hamlet' template into a Happstack 'Response'
--
-- Due to changes in Hamlet, this function is now just defined as:
--
-- > hamletToResponse showFn hamlet = toResponse $ hamlet showFn
--
-- You probably do not need to use this library at all these days.
hamletToResponse :: (url -> [(Text, Text)] -> Text) -- ^ function to 'url' values in the template into their 'String' representation
                 -> Hamlet url  -- ^ a 'Hamlet' template
                 -> Response
hamletToResponse showFn hamlet = 
    toResponse $ hamlet showFn
