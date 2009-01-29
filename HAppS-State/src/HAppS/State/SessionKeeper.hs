{-# LANGUAGE TemplateHaskell, UndecidableInstances, CPP #-}
module HAppS.State.SessionKeeper where


import HAppS.State
import HAppS.State.ComponentTH
import HAppS.State.ComponentTypes
import HAppS.Util.Common
import HAppS.Data

import Data.Typeable
import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Map as M

instance StartState (M.Map a b) where
    startState = return $ M.empty

$(deriveAll [''Read,''Show]
  [d|
      type SessionKey = Integer
      newtype Session a = Session { unSession :: (M.Map SessionKey (a,Seconds,EpochMilli)) }
      data UserComponent key = UserComponent key (Component (Session Int))
      data IxSet a = IxSet a
      type TySynComponent key1 key2 = IxSet (key1,key2)
   |]
 )

instance StartState a => StartState (IxSet a) where
    startState = liftM IxSet startState

methodOne :: (Ord key1, Eq key2) => key2 -> key1 -> Query (TySynComponent key1 key2) ()
methodOne _ _ = return ()

$(methods_ SerializeString ''TySynComponent ['methodOne])
$(atStart ''TySynComponent [])

