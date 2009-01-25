{-# OPTIONS -fglasgow-exts #-}
module HAppS.Server.Cron (cron)
--    (Seconds, CronEvent(..), everyNthSecond,runBackground)
    where

import Control.Concurrent (threadDelay)

type Seconds = Int

cron :: Seconds -> IO () -> IO a
cron seconds action
    = loop
    where loop = do threadDelay (10^(6 :: Int) * seconds)
                    action
                    loop
