{-# OPTIONS -fcontext-stack=25 -fglasgow-exts -fth -fallow-overlapping-instances -fallow-undecidable-instances -cpp #-}
-- LANGUAGE TemplateHaskell, FlexibleInstances,
--             OverlappingInstances, UndecidableInstances 
module HAppS.Server.Facebook where
import Control.Monad.Reader

import Data.Char
import Data.List
import Data.Maybe

import HAppS.Data
import HAppS.Data.Atom (Email(..))
import HAppS.Util.Common
import HAppS.Server.SURI
import HAppS.Server.SimpleHTTP hiding (escape,Method)

import HAppS.Crypto.MD5 ( md5, stringMD5)
import Text.Printf
import System.Directory

import Data.Generics

import qualified Network.Browser as Browser
import qualified Network.HTTP as HTTP
import Control.Monad.Identity

import HAppS.Server.XSLT
import HAppS.Server.HTTP.Types (Response,rsBody)
import qualified Data.ByteString.Lazy.Char8 as L
import System.Time
{--
Conceptually the demo app should expose functionality in website and facebook at once

1. facebook wrapper should take an fbconf and a require app added parameter
2. ServerParts that are depended on fb_Ses@(FB_Ses uid sessionId appadded)
   can we make them implicit parameters so the user doesn't have to see it?
3. if we get the parameters in a post then check valid and assign.
4. if we get parameters in a cookie then check valid and assign.
5. set cookie on initial login
6. calling API functions by typename before _response of returned XML
    Friends_get_response [uid] <- fb $ Fields fs .&. Title t

* make the response types be instances of HasArgs so fb can enforce
* define InCouple class that enforces that a type is inside the couple somewhere.
* enhance xml lib to allow matching if the constructor prefix matches.  Then we get
  modify fb to drop _response in element name before converting.

  Friends_get_response [uid] <- fb $ Fields fs .&. Title t -- and it all typechecks!

also need to get pairs to drop the constructor names if present.
careful to only do so if they are present! e.g. UserName "alex" should become 
userName=alex.  But UserInfo (UserName "alex") should become userName=alex




uids fields title markup
uids1 uids2

so use heterogenous list of things that are convertible 
--}
#ifndef __HADDOCK__
$(deriveAll [''Show,''Read,''Default, ''Eq, ''Ord]
   [d|


    -- data FBSes = FBSes Uid SessionId AppAdded

    --  newtype SessionId = SessionId String
    -- newtype AppAdded = AppAdded Bool
    data Friends_getAppUsers_response = Friends_getAppUsers_response [Uid] 
    data Friends_get_response = Friends_get_response [Uid] 

    newtype Uid = Uid Integer
    newtype Friends = Friends [Integer]
    newtype Uids1 = Uids1 [Integer]
    newtype Uids2 = Uids2 [Integer]
    newtype Uids = Uids [Integer]

    -- used in array posted from multi-friend-input
    newtype Requested = Requested [Ids]
    newtype Ids = Ids Integer 

    newtype Fields = Fields [String]
    newtype Title = Title String
    newtype Method = Method String



    data Fb_config = Fb_config Api_key Secret App_id Canvas_id Admins
    newtype Secret = Secret String
    newtype Api_key = Api_key String
    newtype App_id = App_id Int
    newtype Canvas_id = Canvas_id String
    newtype Admins = Admins [Uid]
    
        
    newtype Session_key=Session_key String 
    newtype Fb_sig_expires=Fb_sig_expires Int

    data FBSession = FBSession 
                     Fb_sig_in_canvas 
                     Fb_sig_added 
                     Fb_sig_time
                     Fb_sig_api_key 
                     Fb_sig
                 ---optional fields
                     Fb_sig_user 
                     Fb_sig_friends
                     Fb_sig_session_key
                     Fb_sig_expires
                     Fb_sig_profile_update_time
                     --stuff that is just carried here
                     (Maybe (XSLTCmd,XSLPath))

    newtype Tbool = Tbool Bool
    newtype Tlist = Tlist [Int]
    data Friend_info = Friend_info Uid Uid Are_friends 
    newtype Are_friends = Are_friends Bool
    data Friends_areFriends_response = Friends_areFriends_response [Friend_info]
    newtype Users_getInfo_response = Users_getInfo_response [User]
    data Profile_setFBML_response = Profile_setFBML_response Bool
    data Profile_getFBML_response = Profile_getFBML_response String

    newtype Notifications_send_response = Notifications_send_response String
    newtype To_ids = To_ids [Integer]
    newtype Notification = Notification String
    data Error_response = Error_response Error_code
    newtype Error_code=Error_code Int
    --newtype Email = Email String


    newtype Fb_sig_in_canvas = Fb_sig_in_canvas Bool 
    newtype Fb_sig_added = Fb_sig_added Bool
    newtype Fb_sig_time = Fb_sig_time Integer
    newtype Fb_sig_api_key = Fb_sig_api_key String
    newtype Fb_sig = Fb_sig String
    newtype Fb_sig_user = Fb_sig_user Integer
    newtype Fb_sig_friends = Fb_sig_friends [Integer] -- add automatic comma sep parse
    newtype Fb_sig_session_key= Fb_sig_session_key String
    newtype Fb_sig_profile_update_time = Fb_sig_profile_update_time Integer

    data User = User Uid (Maybe About_me) Affiliations -- we may need more lets see
    newtype About_me = About_me String
    newtype Affiliations = Affiliations [Affiliation]
    data Affiliation = Affiliation Nid Name Type Status Year
    newtype Nid = Nid Integer
    newtype Name = Name String
    newtype Type = Type String -- should be fixed later to enumeration
    newtype Status = Status String -- not sure what is here
    newtype Year = Year Int

    newtype AppUsers = AppUsers [Uid]
    data InviteInfo = InviteInfo Uid AppUsers BaseURL --- used for producing invites
    data NoFriends = NoFriends
    newtype InstallURL = InstallURL String
    newtype BaseURL = BaseURL String
    |]
 )

instance Version Uid
instance Version Element

$(deriveSerializeFor [ ''Uid
                     , ''Element ])

#endif

a .$ b = b a
fb'::(?fb::Int)=>Int
fb' = ?fb
type FBInfo = (Api_key,Secret,FBSession)

friends_getAppUsers::(?fbSession::FBSession) => IO AppUsers
friends_getAppUsers = 
    do
    Friends_getAppUsers_response uids <- fb (?fbSession::FBSession) ()--(?fbInfo::FBInfo) ()
    return $ AppUsers $ gFind uids

users_getInfo uids fields = 
    do
    Users_getInfo_response users <- fb (?fbSession::FBSession) $ (Uids $ gFind uids) .&. (Fields fields)
    return users

--(networks::Maybe Affiliations) <- 
getNetworks::(?fbSession::FBSession)=>IO (Maybe Affiliations)
getNetworks = return . gFind =<< users_getInfo [uid] ["affiliations"]


--now we want to shift this so that the stuff is rendered in the present layer
notifications_send uids notifObj emailObj =
    do
    print "UIDS="
    print uids
    let (xsltcmd::XSLTCmd
         ,xslpath::XSLPath) = gFind' (?fbSession::FBSession)
        f x = do
              let els = toPublicXml x
              if null els then return "" else do
              let res = toResponse els
              xres <- doXslt xsltcmd xslpath res 
              (return . L.unpack . rsBody) xres
              
        f :: (ToMessage a,Xml a)=> a -> IO String
    (notification::String) <- f notifObj
    (email::String) <- f emailObj
    Notifications_send_response confirm <-
        fb (?fbSession::FBSession) $ 
               (To_ids $ gFind uids) .&.
               (Notification notification) 
               -- .&. (Email email)
               .&. (if null email then Nothing else Just $ Email email)
    print "CONFIRM"
    print confirm
    --error "abc"
    return confirm
    

--data Nofitications_send_response = Nofitications_send_response confirm 

profile_setFBML uid markup =
    do
    --can markup be passed as straight string of fbml?
    --how do we call out to xsl for this?
    Profile_setFBML_response x <- fb (?fbSession::FBSession) $ uid .&. markup
    return x

profile_getFBML uid  =
    do
    Profile_getFBML_response markup <- fb (?fbSession::FBSession) $ uid
    --does this need decoding?
    return markup

friends_areFriends uids1 uids2 =
    do
    Friends_areFriends_response friendInfos <- 
        fb (?fbInfo::FBInfo) $ (Uids1 $ gFind uids1) .&. (Uids2 $ gFind uids2)
    return friendInfos
                                               
getMbUid () = return mbUid
getUid () = getMbUid () >>= return . fromJust
mbUid::(?fbSession::FBSession) => (Maybe Uid)
mbUid = fmap toUid $ gFind ?fbSession
uid::(?fbSession::FBSession) => (Uid)
uid = fromJust mbUid
friends::(?fbSession::FBSession) => (Friends)
friends = fromJust $ fmap toFriends $ gFind (?fbSession::FBSession)
numFriends::(?fbSession::FBSession) => Int
numFriends = let (Friends fs) = friends in length fs

--mbFriends = fmap toUid $ gFind ?fbSession
--time::(?fbSession::FBSession) => Integer
--time = let (Fb_sig_time t) = gFind' ?fbSession in t

getAppURLs = 
    do
    config <- getConfig
    let Api_key key = gFind' config
        Canvas_id cid = gFind' config
        installURL = InstallURL $ 
                     "http://www.facebook.com/install.php?api_key="++key
        baseURL = BaseURL $ "http://apps.facebook/com/"++cid
    return (baseURL,installURL)

getInstallURL::(?fbSession::FBSession) => String -> IO InstallURL
getInstallURL (next) = 
    do
    config <- getConfig
    let Api_key key = gFind' config
    return $ InstallURL $ "http://www.facebook.com/install.php?api_key="++key++
           if null next then "" else "&next="++escape next

getBaseURL = do
             config <- getConfig
             let (Canvas_id cid) = gFind' config
             return $ BaseURL $ "http://apps.facebook/com/"++cid++"/"

--there is no invite info if all user's friends are app users
getInviteInfo::(?fbSession::FBSession) => IO (Maybe InviteInfo)
getInviteInfo = do
                appUsers@(AppUsers apps) <-friends_getAppUsers 
                -- let (Friends fs) = friends
                if numFriends == length apps then return Nothing else do
                return . Just . InviteInfo uid appUsers =<< getBaseURL


                   


isAppAdded::(?fbSession::FBSession) => Bool
isAppAdded = maybe (error "no fb_sig_added") (const appAdded) mbAdded
    where 
    mbAdded = gFind ?fbSession 
    Fb_sig_added appAdded = fromJust mbAdded
--fakeEmail (Uid uid) = show uid ++ "@facebook.com"

getConfig = --yes reading a file is bad but roundtripping to fb sucks too
    do
    let path = "config/facebook.xml"
    fe <- doesFileExist path 
    when (not fe) $ error $ "you need a "++path++ "file of type fb_config"
    configData <- readFile path   -- need error message if this fails
    print configData
    let (fb_config::Fb_config) = runIdentity $ fromString Flexible configData
    return fb_config


getAdmins::IO [Uid]
getAdmins = getConfig >>= return . gFind

getIsAdmin::(?fbSession::FBSession) => IO Bool
getIsAdmin = getAdmins >>= return . elem uid
         

fb fbSession a = 
    mdo
    fb_config <- getConfig
    TOD t _ <- getClockTime
    let req = makeReq t (fbSession,fb_config) a resp
    print "REQ="
    print req
    res <- spost fbserver req
    let body = HTTP.rspBody $ snd res
    print (body::String)
    print "body printed"
    let --mbResp = fromString Rigid body
        --resp = fromJust mbResp
        -- err =  runIdentity $ fromString Flexible body
        resp = runIdentity $ fromString Flexible body
        -- Just (Error_code errCode) = gFind (err::Error_response)
    -- if errCode >0 then error $ show err else do
    --when (isNothing mbResp) $ error "BAD BODY" ++ body
    print resp
    print "resp returned"
    return resp

fbserver=suri $ fromJust $ parse "http://api.facebook.com/restserver.php"


toUid (Fb_sig_user uid) = Uid uid
toFriends (Fb_sig_friends friends) = Friends friends
toSesKey (Fb_sig_session_key s) = Session_key s

makeReq t fbInfo a resp= req
    where
     [example,_] = [defaultValue,resp]
     cons = first toLower $ 
            show $ toConstr example
     fbCmd' = map (\x->if x=='_' then '.' else x) $ "facebook."++ cons
     method = Method $  if ".response" `isSuffixOf` fbCmd' 
              then take (length fbCmd'- (length "_response")) fbCmd'
              else fbCmd'
     sesKey = (fmap toSesKey (gFind fbInfo::Maybe Fb_sig_session_key))
     args = ("v","1.0"):("call_id",show t):
            (toPairs $ apikey .&. method .&. sesKey .&. a)
     s_args = sort args 
     Secret secret = fromJust $ gFind fbInfo
     (apikey::Api_key) = fromJust $ gFind fbInfo
     raw = concatMap (\(x,y)->x++'=':y) s_args ++ (secret)
     sig = stringMD5 (md5 (L.pack raw))
     req = s_args++[("sig",sig)]

{-
md5 = hex . map fromIntegral . MD5.hash . map (fromIntegral.fromEnum) 
hex::[Integer]->String
hex = concatMap (printf "%02x") 
-}

spost u q = Browser.browse $ Browser.request $ Browser.formToRequest $ 
            Browser.Form HTTP.POST u q


consIf False x list = list
consIf _ x list = x:list

fbApp xslproc stylesheet app -- api_key secret app  
    = 
    xslt xslproc stylesheet [ withData fun]
    where
    fun (fbSes::FBSession) = app $ gSet (Just (xslproc,stylesheet)) fbSes
        --where
        --fbInfo = (api_key,secret,fbSes)::FBInfo 

onlyInstalled app = 
    if isAppAdded then app else 
           [uriRest $ \uri ->
                anyRequest $ fbSeeOther . gFind' =<< liftIO (getInstallURL uri)
           -- anyRequest $ fbSeeOther . gFind' =<< getInstallURL ""
           ]

postAdd :: Monad m => [ServerPartT m String]
postAdd = [uriRest $ \uri -> anyRequest $ fbSeeOther uri]
--     = 

fbSeeOther s = ok $ "<fb:redirect url=\""++s++"\"/>"

--ssh_forward host port = unsafePerformIO $ print "hello"
{--
fbapp fbconf handle =
    ReaderT $ \rq -> 
    do 
    
    return "12"
--}



--require args
-- fb (Session_key "abc" .&. Secret "abc") (Uid 123 .&. Title "abc")::IO Friends_get_response
-- Friends_get_response [Uid] <- fb fbInfo ()

--can make it typesafe.  now we just need to deal with response
class HasArgs a b | a -> b 
instance (HasT args Uid,HasT args Title) => HasArgs Friends_get_response args
f::(Default r,HasArgs r args) => args -> r
f a = defaultValue
Friends_get_response tf = f (Uid 123 .&. Title "abc" .&. Secret "abc")

{--
class HasTs targs args 
instance HasTs Nil args
instance (HasTs targs args,HasT args t) =>HasTs (Couple t targs) args
--}

    --data FbInfo = FbInfo Fb_sig_in_canvas Fb_sig_user Fb_sig_session_key
{--
    newtype In_canvas=Fb_sig_in_canvas Bool
    newtype Time=Time Float
    newtype User=User Uid
    newtype Profile_update_time=Profile_update_time Integer

--}

{--    
    newtype Fb_sig_friends=Fb_sig_friends [Uid]
    newtype Fb_sig_api_key=Fb_sig_api_key String
    newtype Fb_sig_added=Fb_sig_added Bool
    newtype Fb_sig=Fb_sig String
--}

