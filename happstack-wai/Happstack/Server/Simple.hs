module Happstack.Server.Simple
    ( simpleHTTP
    , application
    ) where

import Control.Monad (mzero)
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.UTF8       as LU (toString, fromString)
import Happstack.Server.Monads (Request(..), ServerPartT, runWebT, runServerPartT)
import Network.Wai (Application, Response, pathInfo)
import qualified Network.Wai as Wai (Request(pathInfo))
import Network.Wai.Handler.SimpleServer (run)
import Web.Encodings (decodeUrl)
import System.FilePath (splitDirectories)

-- | Execute a serer part on the given port
simpleHTTP :: Int -> ServerPartT IO Response -> IO ()
simpleHTTP port sp = run port $ application sp

-- | Integrate a server part with the wai platform
application :: ServerPartT IO Response -> Application
application sp rq =
    do mRes <- runWebT $ runServerPartT sp $ toRequest rq
       case mRes of
         Nothing -> mzero
         (Just res) -> return res

-- | Convert a WAI request to a Happstack request
toRequest :: Wai.Request -> Request
toRequest wRq = Request wRq $ pathEls $ B.unpack $ pathInfo wRq

-- | Get the path components from a String.
pathEls :: String -> [String]
pathEls = (drop 1) . map decodeUrl . splitDirectories
