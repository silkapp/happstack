{-# LANGUAGE TemplateHaskell, FlexibleInstances, DeriveDataTypeable,
             ImplicitParams, TypeSynonymInstances, TypeFamilies,
             MultiParamTypeClasses, TypeOperators #-}

{- LANGUAGE    TemplateHaskell , FlexibleInstances, UndecidableInstances, OverlappingInstances, 
                DeriveDataTypeable, FlexibleContexts, ImplicitParams,
                MultiParamTypeClasses, TypeSynonymInstances, TypeFamilies -}

-- MultiParamTypeClasses
module HAppS.Store.HelpReqs where

import Control.Monad.State hiding (State)
import Control.Monad.Reader

import HAppS.Data
import HAppS.Data.Atom
import HAppS.Data.IxSet
import HAppS.State
import HAppS.Server.Facebook as FB 
    (Uid,uid,FBSession,fbSeeOther,getIsAdmin
    ,getAdmins,notifications_send  )
import HAppS.Store.Util
import HAppS.Server.SimpleHTTP -- hiding (ok)
import HAppS.Store.FlashMsgs
import HAppS.Server.HTTP.Types (Response)
{--

  Use HelpReqs to track help requests on your app.  The concept is that the
  user just fills out a form describing their needs and you get back to them.

  See FlashMsgs for the cannonical info, but..

--}
#ifndef __HADDOCK__
$( deriveAll [''Show,''Default,''Read,''Eq,''Ord]
   [d|
       -- the data for the help system
       data HelpReqForm = HelpReqForm --get the form
       data HelpReq = HelpReq FB.Uid Published Title HelpText Status--post the help
       newtype HelpText = HelpText String
       data Status = Open | Closed Published
       newtype HelpFeed = HelpFeed [HelpReq] -- provide a help feed
       data HelpMsgReceived = HelpMsgReceived
       --data Status = Open | Assigned FB.Uid Published | Closed FB.Uid Published
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
#endif
{-
$(deriveAll [''Show,''Default,''Read]
  [d| data HelpReqs = HelpReqs {unHelpReqs :: HelpReqs_ } |]
 )
-}

--Command functions
addHelpReq :: HelpReq -> Update HelpReqs ()
addHelpReq helpReq
    = do seconds <- liftM (`div` 1000) getTime
         modify $ insert (gSet (Published seconds) helpReq)
   
getHelpReqs :: Query HelpReqs [HelpReq]
getHelpReqs  = liftM byRevTime ask

#ifndef __HADDOCK__
$(mkMethods ''HelpReqs ['addHelpReq,'getHelpReqs])
instance Component HelpReqs where
    type Dependencies HelpReqs = FlashMsgs HelpMsgReceived :+: End
#endif



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
                               webUpdate $ AddHelpReq helpReq
                               liftIO $ insFlashMsg uid HelpMsgReceived
                               admins <- liftIO $ getAdmins
                               unless (null admins) $ 
                                      do
                                      conf <- liftIO $ notifications_send admins 
                                                      (Title "helpreq")
                                                      ()
                                      return ()
                               liftM toResponse $ fbSeeOther "side-nav"
                   ]]

    ,dir "helps" [method () $ do
                  isAdmin <- liftIO $ getIsAdmin
                  if not isAdmin then forbidden (toResponse "not admin!") else do
                  flashMsg <- liftIO $ (extFlashMsg uid :: IO (Maybe HelpMsgReceived))
                  helpReqs <- webQuery $ GetHelpReqs
                  (ok . toResponse .
                     insEl (Attr "context" "helpfeed") . 
                     insEl flashMsg .
                     HelpFeed . 
                     take 1000) helpReqs 
                 ]
      ]






































--controls
--addHelpReq = update . AddHelpReq'
{--

--addHelpReq' adr = update . adr 

controls::Q [Dec]
controls = return  
           [
--            fun0_1 "addHelpReq" "addHelpReq'" "AddHelpReq"
           fun0_1 "getHelpReqs" "query" "GetHelpReqs"
           ]
--}                    
-- $(return 
--  [fun0_1 "addHelpReq" "addHelpReq'" "AddHelpReq"])           


--OBSOLETE? ---
-- Control functions that need some help
{--
addHelpMsg help@(HelpMsg uid (Published t) msg) = 
    do
    TOD eTime _ <- getClockTime
    if t > eTime || t < eTime - 3600000 then return () else do
    update $ AddHelpMsg help

getHelpMsgs = query GetHelpMsgs
--}
