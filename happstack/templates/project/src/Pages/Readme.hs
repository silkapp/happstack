-- | demonstration of using HStringTemplate to render a page
module Pages.Readme (readmePage) where

import Control.Monad.Trans (liftIO)
import Happstack.Server (Method(GET), Response, ServerPart, methodM)
import Happstack.Server.HStringTemplate (webST)
import Pages.Common (dateStr)
import System.Time (getClockTime)

readmePage :: ServerPart Response
readmePage =
    do methodM GET
       now <- liftIO getClockTime
       webST "readme" [("time", dateStr now)]
