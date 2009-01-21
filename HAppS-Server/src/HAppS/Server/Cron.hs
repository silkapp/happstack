{-# OPTIONS -fglasgow-exts #-}
module HAppS.Server.Cron (cron)
--    (Seconds, CronEvent(..), everyNthSecond,runBackground)
    where

import Control.Concurrent
import Data.Typeable
import HAppS.State
import HAppS.Util.Concurrent
import Control.Monad
import System.Random

type Seconds = Int

cron :: Seconds -> IO () -> IO a
cron seconds action
    = loop
    where loop = do threadDelay (10^6 * seconds)
                    action
                    loop
