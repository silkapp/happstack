{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module Routes (routes) where

import Control.Monad    (msum)
import Happstack.Server ( Browsing(EnableBrowsing), Method(GET), Response, ServerPart
                        , decodeBody, defaultBodyPolicy, dir, methodM, serveDirectory
                        , seeOther, toResponse)
import Pages            (guestBookEntriesPage, postEntryPage, readmePage)

routes :: ServerPart Response
routes = 
    do decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)        -- decode the request body if present. 
       msum [ methodM GET >> seeOther "/guestbook" (toResponse ())  -- redirect / to /guestbook
            , dir "guestbook" $ guestBookRoutes                     -- HSP pages
            , dir "README"    $ readmePage                          -- HStringTemplate example
            , serveDirectory EnableBrowsing ["index.html"] "public" -- static file serving
            ]

guestBookRoutes :: ServerPart Response
guestBookRoutes =
    msum [ guestBookEntriesPage 
         , postEntryPage
         ]
