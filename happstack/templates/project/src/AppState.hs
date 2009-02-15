{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances
    #-}
module AppState where
import Happstack.Data
import Happstack.State
import Control.Monad.Reader
  (ask)
import Control.Monad.State
  (get
  ,put)
import Happstack.State.ClockTime

-- GuestBookEntry: simple guest book entry
$(deriveAll [''Show, ''Eq, ''Ord]
  [d|
      data GuestBookEntry = GuestBookEntry
          { author  :: String
          , message :: String
          , date    :: ClockTime
          }
      
      newtype GuestBook = GuestBook { guestBookEntries :: [GuestBookEntry] }
   |]
 )

$(deriveSerialize ''GuestBookEntry)
instance Version GuestBookEntry

$(deriveSerialize ''GuestBook)
instance Version GuestBook

-- AppState: define our own component, 'AppState' for data persistence
$(deriveAll [''Show, ''Eq, ''Ord]
  [d|
      data AppState = AppState
          { guestBook :: GuestBook
          }
   |])
$(deriveSerialize ''AppState)
instance Version AppState

instance Component AppState where
  type Dependencies AppState = End
  initialValue = AppState (GuestBook []) -- empty list of GuestBookEntry's

-- readGuestBook: get the guestBook from AppState
readGuestBook :: Query AppState GuestBook
readGuestBook = do
  state <- ask
  return $ guestBook state
  
addGuestBookEntry :: GuestBookEntry -> Update AppState  ()
addGuestBookEntry e = do
  st <- get
  let (GuestBook gb) = guestBook st
  put st{guestBook=GuestBook (e:gb)}
  
-- create types for event serialization
$(mkMethods ''AppState ['readGuestBook, 'addGuestBookEntry])
