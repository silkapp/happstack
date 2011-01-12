{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, UndecidableInstances  #-}
module Types.GuestBook where

import Happstack.Data            (Default, Version, deriveAll, deriveSerialize)
import System.Time               (ClockTime)
import Happstack.State.ClockTime () -- instance Data ClockTime

$(deriveAll [''Show, ''Eq, ''Ord, ''Default]
  [d|
      -- |GuestBookEntry: simple guest book entry
      data GuestBookEntry = GuestBookEntry
          { author  :: String
          , message :: String
          , date    :: ClockTime
          , email :: String
          }

      -- |GuestBook: a list of GuestBookEntry
      newtype GuestBook = GuestBook { guestBookEntries :: [GuestBookEntry] }
   |])

$(deriveSerialize ''GuestBookEntry)
instance Version GuestBookEntry

$(deriveSerialize ''GuestBook)
instance Version GuestBook

