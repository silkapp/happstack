module Happstack.Util.Cron (cron) where

import Control.Concurrent (threadDelay)

type Seconds = Int

-- | Given an action f and a number of seconds t, cron will execute
-- f every t seconds with the first execution t seconds after cron is called.
-- cron does not spawn a new thread.
cron :: Seconds -> IO () -> IO a
cron seconds0 action = loop seconds0
    where maxSeconds = (maxBound :: Int) `div`  10^(6 ::Int)
          loop seconds =
            do if seconds <= maxSeconds
                then do threadDelay (10^(6 :: Int) * seconds)
                        action
                        loop seconds0
                else do threadDelay (10^(6 :: Int) * maxSeconds)
                        loop (seconds - maxSeconds)
