module Happstack.Server.Run
    ( serve
    , application
    ) where

import Control.Monad (mzero)
import qualified Data.ByteString.Char8 as B
import Data.Enumerator ()
import Happstack.Server.Monads (Request(..), ServerPartT, runWebT, runServerPartT)
import Network.Wai (Application, Response, rawPathInfo)
import qualified Network.Wai as Wai (Request)
import Web.Encodings (decodeUrl)
import System.FilePath (splitDirectories)

-- | Execute a serer part on the given port
serve :: (Int -> Application -> IO ()) -> Int -> ServerPartT IO Response -> IO ()
serve run' port sp = run' port $ application sp

-- | Integrate a server part with the wai platform
application :: ServerPartT IO Response -> Application
application sp rq =
    do mRes <- runWebT $ runServerPartT sp $ toRequest rq
       case mRes of
         (Just res) -> return res
--         Nothing    -> return (responseLBS 

-- | Convert a WAI request to a Happstack request
toRequest :: Wai.Request -> Request
toRequest wRq = Request wRq $ pathEls $ B.unpack $ rawPathInfo wRq

-- | Get the path components from a String.
pathEls :: String -> [String]
pathEls = (drop 1) . map decodeUrl . splitDirectories
