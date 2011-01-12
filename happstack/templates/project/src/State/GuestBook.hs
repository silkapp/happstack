{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances
    #-}
module State.GuestBook 
    ( GuestBook(..)
    , ReadGuestBook(..)
    , AddGuestBookEntry(..)
    ) where

import Happstack.State      (Component(..), Dependencies, End, Query, Update, mkMethods)
import Control.Monad.Reader (ask)
import Control.Monad.State  (modify)
import Types.GuestBook      (GuestBook(..), GuestBookEntry(..))

-- | get the 'GuestBook'
readGuestBook :: Query GuestBook GuestBook
readGuestBook = ask
  
-- | add a 'GuestBookEntry' to the 'GuestBook'
addGuestBookEntry :: GuestBookEntry -> Update GuestBook ()
addGuestBookEntry e = modify $ \(GuestBook gb) -> (GuestBook (e:gb))

-- |make Guestbook its own Component
instance Component GuestBook where
  type Dependencies GuestBook = End
  initialValue = GuestBook []
  
-- create types for event serialization
$(mkMethods ''GuestBook ['readGuestBook, 'addGuestBookEntry])
