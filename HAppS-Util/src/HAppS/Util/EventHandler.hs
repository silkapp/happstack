{-# OPTIONS -fglasgow-exts #-}
module HAppS.Util.EventHandler where

--import HAppS.MACID

{-
-- | Create an event handler for the 'dispatch' type which must be unique
--   and is used as base for dispatch. The Proxy argument is used only to
--   select the correct instance and should be ignored.
--
--   EventHandler is used by HAppS.Agents.WithBlockingIO and HAppS.Agents.SessionKeeperEx.
class EventHandler dispatch st ev res | dispatch -> st ev res where
    eventHandler :: Proxy dispatch -> Ev st ev res

data DoNothing st ev

instance EventHandler (DoNothing st ev) st ev () where
    eventHandler _ = return ()

-}
