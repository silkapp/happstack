module Happstack.Server.Simple where

import Control.Monad (mzero)
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.UTF8       as LU (toString, fromString)
import Happstack.Server.Monads (ServerPartT, runWebT, runServerPartT)
import Network.Wai (Request(pathInfo), Response)
import Network.Wai.Handler.SimpleServer (run)
import Web.Encodings (decodeUrl)
import System.FilePath (splitDirectories)

simpleHTTP :: Int -> ServerPartT IO Response -> IO ()
simpleHTTP port sp = run port app
  where
    app :: Request -> IO Response
    app rq = 
      do mRes <- runWebT $ runServerPartT sp (pathEls (B.unpack (pathInfo rq)), rq)
         case mRes of
           Nothing -> mzero
           (Just res) -> return res

-- | Get the path components from a String.
pathEls :: String -> [String]
pathEls = (drop 1) . map decodeUrl . splitDirectories
