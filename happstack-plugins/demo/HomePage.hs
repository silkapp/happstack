module HomePage where

import Happstack.Server

helloPage :: String -> ServerPart Response
helloPage noun = ok (toResponse $ "hello, " ++ noun)
