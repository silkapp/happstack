-----------------------------------------------------------------------------
-- |
-- Module      :  Happstack.Util.Mail
-- Copyright   :  Happstack.com 2009
-- License     :  BSD3
--
-- Maintainer  :  Matthew Elder
-- Stability   :  provisional
-- Portability :  linux/windows
--
-- Mail is a simple library with which you can add email functionality to your
-- application. It assumes you have access to a smarthost which can relay all
-- your mail.
-- 
-- As an example:
--
-- > import Happstack.Util.Mail
-- > main :: IO ()
-- > main = do
-- >     sendSimpleMessages "10.2.23.11" "example.com" [message]
-- >     where message = SimpleMessage
-- >                         [NameAddr (Just "John Doe") "johnd@example.com"]
-- >                         [NameAddr (Just "Patch-Tag Team") "team@patch-tag.com"]
-- >                         "My test email using Happstack.Util.Mail"
-- >                         "Hi, this is a test email which uses Happstack."

module Happstack.Util.Mail
    ( NameAddr(..)
    , SimpleMessage(..)
    , sendRawMessages
    , sendSimpleMessages
    ) where

import Data.IORef (newIORef, readIORef)
import Network.Socket
    (SockAddr(..)
    , inet_addr
    )
import Network.SMTP.Client
import System.Log.Logger (Priority(..), logM)
import System.Time
    ( CalendarTime(..)
    , getClockTime
    , toCalendarTime
    )

data SimpleMessage
    = SimpleMessage
        { from :: [NameAddr] -- ^ The sender(s)
        , to :: [NameAddr]   -- ^ The recipient(s)
        , subject :: String  -- ^ The subject line
        , body :: String     -- ^ The body
        }
    deriving (Show)

toMessage :: CalendarTime -> SimpleMessage -> Message
toMessage ct sm =
    Message
        [From (from sm), To (to sm), Subject (subject sm), Date ct]
        (body sm)
        
log' :: Priority -> String -> IO ()
log' = logM "Happstack.Util.Mail"

-- | Simplest way to send mail.  Takes the smarthost ip, the HELO domain, and a list of SimpleMessage.
sendSimpleMessages :: String          -- ^ IP address of the smarthost
                   -> String          -- ^ HELO domain (should be the same as your from-address-domain)
                   -> [SimpleMessage] -- ^ List of simple messages to send
                   -> IO ()
sendSimpleMessages smartHostIp heloDomain simpleMessages = do
    nowCT <- toCalendarTime =<< getClockTime
    hostAddr <- inet_addr smartHostIp
    let smtpSockAddr = SockAddrInet 25 hostAddr
    sendRawMessages smtpSockAddr heloDomain (map (toMessage nowCT) simpleMessages)

-- | Use this if you need more control than sendSimpleMessages gives you.
sendRawMessages :: SockAddr  -- ^ SockAddr for the smarthost
                -> String    -- ^ HELO domain (should be the same as your from-address-domain)
                -> [Message] -- ^ List of messages to send
                -> IO ()
sendRawMessages smtpSockAddr heloDomain messages = do
    log' NOTICE $ "connecting to SMTP smarthost: " ++ show smtpSockAddr
    sentRef <- newIORef []
    sendSMTP' (log' INFO) (Just sentRef) heloDomain smtpSockAddr messages
    statuses <- readIORef sentRef
  
    -- If no exception was caught, statuses is guaranteed to be
    -- the same length as the list of input messages, therefore head won't fail here.
    log' NOTICE $ "attempting to send messages:\n" ++ show messages
    case head statuses of
        Nothing ->
            return ()
        Just status ->
            log' ERROR $ "message failed: " ++ show status
