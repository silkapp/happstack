module HomePage where

import Happstack.Server
import Types

greetingPage :: Greeting -> String -> ServerPart Response
greetingPage (Greeting greeting) noun 
    = ok (toResponse $ greeting ++ ", " ++ noun)
