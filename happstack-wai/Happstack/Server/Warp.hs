module Happstack.Server.Warp
    ( warp
    ) where

import Happstack.Server.Run     (serve)
import Happstack.Server.Monads  (ServerPartT)
import Network.Wai              (Response)
import Network.Wai.Handler.Warp (run)

-- | Execute a server part on the given port
warp :: Int -> ServerPartT IO Response -> IO ()
warp port sp = serve run port sp
