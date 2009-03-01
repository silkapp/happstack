{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances
    #-}
module GuestBook.State where
import Happstack.Data
import Happstack.State
import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import Happstack.State.ClockTime

$(deriveAll [''Show, ''Eq, ''Ord, ''Default]
  [d|
      -- |GuestBookEntry: simple guest book entry
      data GuestBookEntry = GuestBookEntry
          { author  :: String
          , message :: String
          , date    :: ClockTime
          }

      -- |GuestBook: a list of GuestBookEntry
      newtype GuestBook = GuestBook { guestBookEntries :: [GuestBookEntry] }
   |])

$(deriveSerialize ''GuestBookEntry)
instance Version GuestBookEntry

$(deriveSerialize ''GuestBook)
instance Version GuestBook

-- | get the 'GuestBook'
readGuestBook :: Query GuestBook GuestBook
readGuestBook = ask
  
-- | add a 'GuestBookEntry' to the 'GuestBook'
addGuestBookEntry :: GuestBookEntry -> Update GuestBook ()
addGuestBookEntry e = modify $ \(GuestBook gb) -> (GuestBook (e:gb))

-- |make Guestbook its own Component
instance Component GuestBook where
  type Dependencies GuestBook = End
  initialValue = defaultValue
  
-- create types for event serialization
$(mkMethods ''GuestBook ['readGuestBook, 'addGuestBookEntry])
