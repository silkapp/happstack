{-# LANGUAGE CPP, TemplateHaskell, FlexibleInstances, DeriveDataTypeable, FlexibleContexts,
             MultiParamTypeClasses, TypeFamilies, TypeSynonymInstances, UndecidableInstances #-}

module HAppS.Store.FlashMsgs where

import Control.Monad.State hiding (State)
import Control.Monad.Reader


import HAppS.Data
import Data.Generics.Basics
import HAppS.Data.Atom
import HAppS.Data.IxSet
import HAppS.State
import HAppS.Server.Facebook as FB



{--
  Use FashMsgs to send one off messages to the user.  Think of it as single use sessions.
  Note: we don't have Sessions here yet because we need a nicer expose function
  that lets us have multiple sessions in state and parametrize by session value
  Also not sure that sessions are useful in this infrastructure

  It would also be nice to be able to parametrize on the Uid, but for now we treat the
  facebook Uid type as the universal Id

  1. Put a FlashMsgs somewhere in your state
  2. Make state an instance of class HasFlash

     instance HasFlash State where
        withFlashMsgs = State.withFlashMsgs
        flashMsgs = State.flashMsgs

  3. put commmands in your getInterface function in State

  Now you can use insFlashMsg and extFlashMsg from your app.
  If we wanted a cleanup cycle we could add a periodic handler to stdMain
    stdMain () $ simpleHTTP impl $ periodic 10 impl $

Improvements:
  * get rid of HasFlash and use syb/generic haskell for this stuff
  * infer state getinterface rather than manual addition
--}


--Types
$( deriveAll [''Read,''Show,''Default,''Eq,''Ord]
   [d|

       -- we are also going to want to issue flash msgs
       data FlashMsg msg = FlashMsg FB.Uid Published msg -- FlashContent
       newtype FlashContent = FlashContent [Element]

    |]
 )

instance Version (FlashMsg msg)
instance Version FlashContent

$(deriveSerialize ''FlashMsg)
$(deriveSerialize ''FlashContent)

$(inferIxSet "FlashMsgs" ''FlashMsg 'noCalcs [''FB.Uid,''Published])

--Command Functions
setFlashMsg :: (Ord msg,Data msg) => Uid -> msg -> Update (FlashMsgs msg) ()
setFlashMsg u_id msg =
    do
    t <- getTime
    let aux = FlashMsg u_id (Published t) msg
    modify (updateIx u_id aux)
    --delete old messages
    expired <-gets (toList . (@< (Updated $ t-maxAge)))
    mapM (modify . delete) expired
    return ()
    where maxAge = 3600


getFlashMsg :: (Ord msg, Data msg) => Uid -> Query (FlashMsgs msg) (Maybe msg) 
getFlashMsg u_id = 
    (return . gFind . getOne . (@=u_id)) =<< ask

delFlashMsg :: (Ord msg, Data msg) => Uid -> Proxy (FlashMsgs msg) -> Update (FlashMsgs msg) ()
delFlashMsg u_id _ = do
    mbMsg <- gets (getOne . (@=u_id) ) 
    maybe (return ()) (modify . delete) mbMsg

$(mkMethods ''FlashMsgs [ 'setFlashMsg
                        , 'getFlashMsg
                        , 'delFlashMsg ])

instance (Serialize (FlashMsgs a), Ord a, Data a) => Component (FlashMsgs a) where
    initialValue = error "No initialValue for Component (FlashMsgs a) "
    type Dependencies (FlashMsgs a) = End

-- Controls
insFlashMsg :: (Xml a, MonadIO m) => Uid -> a -> m ()
insFlashMsg u_id msg = update $ SetFlashMsg u_id $ toXml msg
extFlashMsg :: (Data msg,
                Serialize msg,
                Ord msg,
                MonadIO m) =>
               Uid -> m (Maybe msg)
extFlashMsg u_id = do
                  msg <- query $ GetFlashMsg u_id
                  let mkProxy :: Maybe msg -> Proxy (FlashMsgs msg)
                      mkProxy _ = Proxy
                  update $ DelFlashMsg u_id (mkProxy msg)
                  return msg

