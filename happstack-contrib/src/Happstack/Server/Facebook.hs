{-# OPTIONS -fcontext-stack=25 #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances, CPP, ImplicitParams, RecursiveDo, DeriveDataTypeable, PatternSignatures,
             ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies, OverlappingInstances, UndecidableInstances, FlexibleContexts #-}
module Happstack.Server.Facebook where
import Control.Monad.Reader

import Data.Char
import Data.List
import Data.Maybe

import Happstack.Data
import Happstack.Contrib.Atom (Email(..))
import Happstack.Contrib.HList
import Happstack.Server.SURI
import Network.URI (URI)
import Happstack.Server.SimpleHTTP hiding (escape,Method)

import Happstack.Crypto.MD5 ( md5, stringMD5)
import System.Directory

import Data.Generics

import qualified Network.Browser as Browser
import qualified Network.HTTP as HTTP
import Control.Monad.Identity

import Happstack.Server.XSLT
import Happstack.Server.HTTP.Types (Response,rsBody)
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

$(deriveAll [''Show,''Read,''Default, ''Eq, ''Ord]
   [d|


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


(.$) :: a -> (a -> b) -> b
a .$ b = b a
fb'::(?fb::Int)=>Int
fb' = ?fb
type FBInfo = (Api_key,Secret,FBSession)

friends_getAppUsers::(?fbSession::FBSession) => IO AppUsers
friends_getAppUsers = 
    do
    Friends_getAppUsers_response uids <- fb (?fbSession::FBSession) ()
    return $ AppUsers $ gFind uids

users_getInfo :: (?fbSession::FBSession, Data a) => a -> [String] -> IO [User]
users_getInfo uids fields = 
    do
    Users_getInfo_response users <- fb (?fbSession::FBSession) $ (Uids $ gFind uids) .&. (Fields fields)
    return users

getNetworks::(?fbSession::FBSession)=>IO (Maybe Affiliations)
getNetworks = return . gFind =<< users_getInfo [uid] ["affiliations"]


--now we want to shift this so that the stuff is rendered in the present layer
notifications_send :: (ToMessage b,
                       Xml b,
                       ToMessage c,
                       Xml c,
                       ?fbSession::FBSession,
                       Data a,
                       Show a) =>
                      a -> b -> c -> IO String
notifications_send uids notifObj emailObj =
    do
    print "UIDS="
    print uids
    let (xsltcmd::XSLTCmd
         ,xslpath::XSLPath) = gFind' (?fbSession::FBSession)
        aux x = do
              let els = toPublicXml x
              if null els then return "" else do
              let res = toResponse els
              xres <- doXslt xsltcmd xslpath res 
              (return . L.unpack . rsBody) xres
              
        aux :: (ToMessage a,Xml a)=> a -> IO String
    (notification::String) <- aux notifObj
    (email::String) <- aux emailObj
    Notifications_send_response confirm <-
        fb (?fbSession::FBSession) $ 
               (To_ids $ gFind uids) .&.
               (Notification notification) 
               -- .&. (Email email)
               .&. (if null email then Nothing else Just $ Email email)
    print "CONFIRM"
    print confirm
    return confirm
    
profile_setFBML :: (?fbSession::FBSession,
                    Xml a,
                    Xml b,
                    Show a,
                    Show b,
                    Data a,
                    Data b,
                    Eq a,
                    Eq b) =>
                   a -> b -> IO Bool
profile_setFBML u_id markup =
    do
    --can markup be passed as straight string of fbml?
    --how do we call out to xsl for this?
    Profile_setFBML_response x <- fb (?fbSession::FBSession) $ u_id .&. markup
    return x

profile_getFBML :: (?fbSession::FBSession,
                    Xml a,
                    Show a,
                    Data a,
                    Eq a) =>
                   a -> IO String
profile_getFBML u_id  =
    do
    Profile_getFBML_response markup <- fb (?fbSession::FBSession) $ u_id
    --does this need decoding?
    return markup

friends_areFriends :: (?fbInfo::FBInfo, Data a, Data b) =>
                      a -> b -> IO [Friend_info]
friends_areFriends uids1 uids2 =
    do
    Friends_areFriends_response friendInfos <- 
        fb (?fbInfo::FBInfo) $ (Uids1 $ gFind uids1) .&. (Uids2 $ gFind uids2)
    return friendInfos
                                               
getMbUid :: (?fbSession::FBSession, Monad m) => () -> m (Maybe Uid)
getMbUid () = return mbUid
getUid :: (?fbSession::FBSession, Monad m) => () -> m Uid
getUid () = getMbUid () >>= return . fromJust
mbUid::(?fbSession::FBSession) => (Maybe Uid)
mbUid = fmap toUid $ gFind ?fbSession
uid::(?fbSession::FBSession) => (Uid)
uid = fromJust mbUid
friends::(?fbSession::FBSession) => (Friends)
friends = fromJust $ fmap toFriends $ gFind (?fbSession::FBSession)
numFriends::(?fbSession::FBSession) => Int
numFriends = let (Friends fs) = friends in length fs

getAppURLs :: IO (BaseURL, InstallURL)
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

getBaseURL :: IO BaseURL
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

getConfig :: IO Fb_config
getConfig = --yes reading a file is bad but roundtripping to fb sucks too
    do
    let configPath = "config/facebook.xml"
    fe <- doesFileExist configPath 
    when (not fe) $ error $ "you need a "++configPath++ "file of type fb_config"
    configData <- readFile configPath   -- need error message if this fails
    print configData
    let (fb_config::Fb_config) = runIdentity $ fromString Flexible configData
    return fb_config


getAdmins::IO [Uid]
getAdmins = getConfig >>= return . gFind

getIsAdmin::(?fbSession::FBSession) => IO Bool
getIsAdmin = getAdmins >>= return . elem uid
         

fb :: (Show a,
       FromString a,
       Xml b,
       Show b,
       Data b,
       Eq b,
       Data a,
       Default a,
       Data t) =>
      t -> b -> IO a
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
    let resp = runIdentity $ fromString Flexible body
    print resp
    print "resp returned"
    return resp

fbserver :: Network.URI.URI
fbserver=suri $ fromJust $ parse "http://api.facebook.com/restserver.php"


toUid :: Fb_sig_user -> Uid
toUid (Fb_sig_user u_id) = Uid u_id
toFriends :: Fb_sig_friends -> Friends
toFriends (Fb_sig_friends fs) = Friends fs
toSesKey :: Fb_sig_session_key -> Session_key
toSesKey (Fb_sig_session_key s) = Session_key s

makeReq :: (Xml b,
            Show b,
            Data b,
            Eq b,
            Show a,
            Data t,
            Default t,
            Data a1) =>
           a -> a1 -> b -> t -> [([Char], [Char])]
makeReq t fbInfo a resp= req
    where
     [example,_] = [defaultValue,resp]
     cons = first toLower $ 
            show $ toConstr example
     fbCmd' = map (\x->if x=='_' then '.' else x) $ "facebook."++ cons
     meth = Method $  if ".response" `isSuffixOf` fbCmd' 
              then take (length fbCmd'- (length "_response")) fbCmd'
              else fbCmd'
     sesKey = (fmap toSesKey (gFind fbInfo::Maybe Fb_sig_session_key))
     args = ("v","1.0"):("call_id",show t):
            (toPairs $ apikey .&. meth .&. sesKey .&. a)
     s_args = sort args 
     Secret secret = fromJust $ gFind fbInfo
     (apikey::Api_key) = fromJust $ gFind fbInfo
     raw = concatMap (\(x,y)->x++'=':y) s_args ++ (secret)
     sig = stringMD5 (md5 (L.pack raw))
     req = s_args++[("sig",sig)]

spost :: URI -> [(String,String)] -> IO (URI, HTTP.Response String)
spost u q = Browser.browse $ Browser.request $ Browser.formToRequest $ 
            Browser.Form HTTP.POST u q


consIf :: Bool -> a -> [a] -> [a]
consIf False _ list = list
consIf _ x list = x:list

fbApp :: (MonadIO m, ToMessage r, ServerMonad m, MonadPlus m) =>
         XSLTCmd
         -> XSLPath
         -> (FBSession -> m r)
         -> m Response
fbApp xslproc stylesheet app -- api_key secret app  
    = 
    xslt xslproc stylesheet $ withData fun
    where
    fun (fbSes::FBSession) = app $ gSet (Just (xslproc,stylesheet)) fbSes

onlyInstalled :: (?fbSession::FBSession, MonadIO m, FilterMonad Response m, ServerMonad m) =>
                 [m [Char]] -> [m [Char]]
onlyInstalled app = 
    if isAppAdded then app else 
           [uriRest $ \uri ->
                fbSeeOther . gFind' =<< liftIO (getInstallURL uri)
           ]

postAdd :: (Monad m, ServerMonad m, FilterMonad Response m) => [m String]
postAdd = [uriRest $ \uri -> fbSeeOther uri]
--     = 

fbSeeOther :: (Monad m, FilterMonad Response m) => [Char] -> m [Char]
fbSeeOther s = ok $ "<fb:redirect url=\""++s++"\"/>"

--can make it typesafe.  now we just need to deal with response
class HasArgs a b | a -> b 
instance (HasT args Uid,HasT args Title) => HasArgs Friends_get_response args
f::(Default r,HasArgs r args) => args -> r
f _ = defaultValue
tf :: [Uid]
Friends_get_response tf = f (Uid 123 .&. Title "abc" .&. Secret "abc")
