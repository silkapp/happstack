{-# LANGUAGE TemplateHaskell, FlexibleInstances, DeriveDataTypeable,
             ImplicitParams, TypeSynonymInstances, TypeFamilies,
             MultiParamTypeClasses, TypeOperators, FlexibleContexts,
             UndecidableInstances #-}

module Happstack.Store.HelpReqs where

import Control.Monad.State hiding (State)
import Control.Monad.Reader

import Happstack.Data
import Happstack.Data.IxSet
import Happstack.Contrib.Atom
import Happstack.State
import Happstack.Server.Facebook as FB 
    (Uid,uid,FBSession,fbSeeOther,getIsAdmin
    ,getAdmins,notifications_send  )
import Happstack.Server.SimpleHTTP 
import Happstack.Store.FlashMsgs
import Happstack.Server.HTTP.Types (Response)

-- Kept from old Happstack.Store.Util
byTime::(Typeable a) => IxSet a -> [a]
byTime = concatMap (\(Published _,es)->es) . groupBy
byRevTime::(Typeable a) => IxSet a -> [a]
byRevTime = concatMap (\(Published _,es)->es) . rGroupBy


{--

  Use HelpReqs to track help requests on your app.  The concept is that the
  user just fills out a form describing their needs and you get back to them.

  See FlashMsgs for the cannonical info, but..

--}
$( deriveAll [''Show,''Default,''Read,''Eq,''Ord]
   [d|
       -- the data for the help system
       data HelpReqForm = HelpReqForm --get the form
       data HelpReq = HelpReq FB.Uid Published Title HelpText Status--post the help
       newtype HelpText = HelpText String
       data Status = Open | Closed Published
       newtype HelpFeed = HelpFeed [HelpReq] -- provide a help feed
       data HelpMsgReceived = HelpMsgReceived
       
     |])

instance Version HelpReq
instance Version Status
instance Version HelpFeed
instance Version HelpText
instance Version HelpMsgReceived
$(deriveSerialize ''HelpReq)
$(deriveSerialize ''Status)
$(deriveSerialize ''HelpFeed)
$(deriveSerialize ''HelpText)
$(deriveSerialize ''HelpMsgReceived)

$(inferIxSet "HelpReqs" ''HelpReq 'noCalcs [''FB.Uid,''Published,''Status] )

--Command functions
addHelpReq :: HelpReq -> Update HelpReqs ()
addHelpReq helpReq
    = do seconds <- liftM (`div` 1000) getTime
         modify $ insert (gSet (Published seconds) helpReq)
   
getHelpReqs :: Query HelpReqs [HelpReq]
getHelpReqs  = liftM byRevTime ask

$(mkMethods ''HelpReqs ['addHelpReq,'getHelpReqs])
instance Component HelpReqs where
    initialValue = error "initialValue not defined for Component HelpReqs"
    type Dependencies HelpReqs = FlashMsgs HelpMsgReceived :+: End



-- http stuff 
http::( ?fbSession::FBSession
      , MonadIO m
      ) => [ServerPartT m Response]
http = 
    [
     dir "help" [ method () $ ok $ toResponse HelpReqForm]

    ,dir "addHelp" 
             [withData $ \helpReq -> 
                  [method () $ do
                               (liftIO . update) $ AddHelpReq helpReq
                               liftIO $ insFlashMsg uid HelpMsgReceived
                               admins <- liftIO $ getAdmins
                               unless (null admins) $ 
                                      do
                                      liftIO $ notifications_send admins 
                                                      (Title "helpreq")
                                                      ()
                                      return ()
                               liftM toResponse $ fbSeeOther "side-nav"
                   ]]

    ,dir "helps" [method () $ do
                  isAdmin <- liftIO $ getIsAdmin
                  if not isAdmin then forbidden (toResponse "not admin!") else do
                  flashMsg <- liftIO $ (extFlashMsg uid :: IO (Maybe HelpMsgReceived))
                  helpReqs <- (liftIO . query) $ GetHelpReqs
                  (ok . toResponse .
                     insEl (Attr "context" "helpfeed") . 
                     insEl flashMsg .
                     HelpFeed . 
                     take 1000) helpReqs 
                 ]
      ]
