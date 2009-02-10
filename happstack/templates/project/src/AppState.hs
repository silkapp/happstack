{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module AppState where
import Happstack.State
import Data.Generics
import Control.Monad.Reader
  (ask)
import Control.Monad.State
  (get
  ,put)

-- GuestBookEntry: simple guest book entry
data GuestBookEntry = GuestBookEntry
  { author :: String
  , message :: String
  } deriving (Show, Read, Ord, Eq, Typeable, Data)
$(deriveSerialize ''GuestBookEntry)
instance Version GuestBookEntry

-- AppState: define our own component, 'AppState' for data persistence
data AppState = AppState
  { guestBook :: [GuestBookEntry]
  } deriving (Show, Read, Ord, Eq, Typeable, Data)
$(deriveSerialize ''AppState)
instance Version AppState
instance Component AppState where
  type Dependencies AppState = End
  initialValue = AppState [] -- empty list of GuestBookEntry's

-- readGuestBook: get the guestBook from AppState
readGuestBook :: Query AppState [GuestBookEntry]
readGuestBook = do
  state <- ask
  return $ guestBook state
  
addGuestBookEntry :: GuestBookEntry -> Update AppState  ()
addGuestBookEntry e = do
  st <- get
  let gb = guestBook st
  put st{guestBook=e:gb}
  
-- create types for event serialization
$(mkMethods ''AppState ['readGuestBook, 'addGuestBookEntry])
