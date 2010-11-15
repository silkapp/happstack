-- | functions for using Heist with Happstack
--
-- See the Heist Section of the Happstack Crash Course for detailed documentation:
--
--  <http://happstack.com/docs/crashcourse/Templates.html#helloheist>
module Happstack.Server.Heist 
    ( templateReloader
    , templateServe
    , render
    ) where

import Control.Monad                           (MonadPlus(mzero), msum)
import Control.Monad.Trans                     (MonadIO)
import           Data.ByteString.Char8         (ByteString)
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy          as L
import Happstack.Server                        (Response, ServerMonad, askRq, nullDir, rqPaths, toResponseBS)
import System.FilePath                         (joinPath)
import Text.Templating.Heist                   (renderTemplate)
import Text.Templating.Heist.TemplateDirectory (TemplateDirectory, getDirectoryTS, reloadTemplateDirectory)
import qualified  Text.XML.Expat.Tree          as X

-- | serve the heist templates from the 'TemplateDirectory m'
templateServe :: (ServerMonad m, MonadPlus m, MonadIO m) =>
                 TemplateDirectory m 
              -> m Response
templateServe td = 
    msum [ nullDir >> render td (B.pack "index")
         , do rq <- askRq
              let safepath = joinPath $ filter (\x->not (null x) && x /= ".." && x /= ".") (rqPaths rq)
              render td (B.pack safepath)
         ]

-- | force a reload of the templates from disk
templateReloader :: (MonadIO m, MonadIO n) => 
                    TemplateDirectory m 
                 -> n Response
templateReloader td = do
    e <- reloadTemplateDirectory td
    return $ toResponseBS (B.pack "text/plain; charset=utf-8") $
        L.fromChunks [either B.pack (const $ B.pack "Templates loaded successfully.") e]


-- | render the specified template
render:: (MonadPlus m, MonadIO m) => 
         TemplateDirectory m  -- ^ 'TemplateDirectory' handle
      -> ByteString -- ^ template name
      -> m Response
render td template = do
    ts    <- getDirectoryTS td
    bytes <- renderTemplate ts template
    flip (maybe mzero) bytes $ \x -> do
        return (toResponseBS (B.pack "text/html; charset=utf-8") (L.fromChunks [x]))
