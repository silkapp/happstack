{-# OPTIONS -fglasgow-exts #-}
module HAppS.Server.MessageWrap where
{-
    (ToMessage(..), toMessage, FromMessage(..),
--     Index(..), 
     reqPath, pathEls, -- getPath,
     ReadString(..),
     look, lookS, lookM, lookMb,
     lookMbRead, mbRead, lookInput
    ,bodyToMap,bodyToList
    ) where
-}

import Control.Monad.Identity
import qualified Data.ByteString.Char8 as P
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.List as List
import qualified Data.Map as M
import Data.Maybe
import HAppS.Server.HTTP.Types as H
import HAppS.Server.JSON
import HAppS.Server.MinHaXML
import HAppS.Server.HTTP.Multipart
import HAppS.Server.SURI as SURI
import HAppS.Util.Common
import qualified Text.XML.HaXml.Types as HaXml
import Data.Generics

{-
class ToMessage x where
    toMessageM      :: Monad m => x -> m H.Result
    toMessageM x = do s <- toMessageBodyM x
                      H.sresult' 200 [s]
    toMessageBodyM  :: Monad m => x -> m P.ByteString
    toMessageBodyM x = (P.concat . H.rsBody) `liftM` toMessageM x

-- | Non-monadic variant of toMessageM, mostly for backwards support.
toMessage :: ToMessage x => x -> Result
toMessage x = runIdentity $ toMessageM x

class FromMessage x where
    fromMessage :: H.Request -> x
    fromMessage msg = x where Identity x = fromMessageM msg
    fromMessageM :: (Monad m,Functor m) => H.Request -> m x
    fromMessageM = return . fromMessage


instance ToMessage H.Result where toMessageM = return
instance ToMessage String where toMessageM   = H.sresult 200

instance ToMessage ([(String,String)],String) where 
    toMessageM (hdrs,val) = 
        do 
        msg <- toMessageM val
        return $ foldr (\ (n,v) res ->setHeader n v res) msg hdrs



instance FromMessage ()        where fromMessage = const ()
instance FromMessage H.Request where fromMessage = id

instance (FromMessage m,FromMessage a) => FromMessage (m,a) where
    fromMessageM m = liftM2 (,) (fromMessageM m) (fromMessageM m)

instance FromMessage m => FromMessage (Maybe m) where
    fromMessageM m = return $ fromMessageM m

-}

{--instance (ToElement x)=>ToMessage (XML x) where 
    toMessageM (XML style obj) = liftM (addHeader "Content-Type" $ ctype el)
				      (toMessageM doc)
        where
        doc = (if ctype el == "text/html" then simpleDoc' NoStyle
              else simpleDoc style) el
        el = HaXml.Elem "alksjd" [] []
        el' = toElement obj
        ctype (HaXml.Elem "html" _ _) = "text/html"
        ctype _ = "text/xxxxml"
        -- ctype (HaXml.Elem "html" _ _) = error "html!!!"


instance (ToJSON x) => ToMessage (JSONCall x) where
    toMessageM (JCall cb x) = liftM (addHeader "Content-Type" "text/javascript")
                                   (toMessageM $ concat [cb,"(",jsonToString $ toJSON x,")"])
--}




{-
bodyToList :: Request -> [(String, String)]
bodyToList = map unInput . bodyToList'
    where unInput (name, input) = (name, P.unpack $ inputValue input)

bodyToList' :: Request -> [(String, Input)]
bodyToList' req = queryInput req ++ bodyInput req

bodyToMap :: Request -> M.Map String String
bodyToMap = M.fromList . bodyToList

bodyToMap' :: Request -> M.Map String Input
bodyToMap' = M.fromList . bodyToList'
-}

queryInput :: SURI -> [(String, Input)]
queryInput uri = formDecode (case SURI.query $ uri of
                               '?':r -> r
                               xs    -> xs)

bodyInput :: Request -> [(String, Input)]
bodyInput req | rqMethod req /= POST = []
bodyInput req =
    let ctype = getHeader "content-type" req >>= parseContentType . P.unpack
        getBS (Body bs) = bs
    in decodeBody ctype (getBS $ rqBody req)


-- Decodes application\/x-www-form-urlencoded inputs.      
formDecode :: String -> [(String, Input)]
formDecode [] = []
formDecode qString = 
    if null pairString then rest else 
           (SURI.unEscape name,simpleInput $ SURI.unEscape val):rest
    where (pairString,qString')= split (=='&') qString
          (name,val)=split (=='=') pairString
          rest=if null qString' then [] else formDecode qString'

decodeBody :: Maybe ContentType
           -> L.ByteString
           -> [(String,Input)]
decodeBody ctype inp
    = case ctype of
        Just (ContentType "application" "x-www-form-urlencoded" _)
            -> formDecode (L.unpack inp)
        Just (ContentType "multipart" "form-data" ps)
            -> multipartDecode ps inp
        Just _ -> [] -- unknown content-type, the user will have to
                     -- deal with it by looking at the raw content
        -- No content-type given, assume x-www-form-urlencoded
        Nothing -> formDecode (L.unpack inp)

-- | Decodes multipart\/form-data input.
multipartDecode :: [(String,String)] -- ^ Content-type parameters
                -> L.ByteString        -- ^ Request body
                -> [(String,Input)]  -- ^ Input variables and values.
multipartDecode ps inp =
    case lookup "boundary" ps of
         Just b -> case parseMultipartBody b inp of
                        Just (MultiPart bs) -> map bodyPartToInput bs
                        Nothing -> [] -- FIXME: report parse error
         Nothing -> [] -- FIXME: report that there was no boundary

bodyPartToInput :: BodyPart -> (String,Input)
bodyPartToInput (BodyPart hs b) =
    case getContentDisposition hs of
              Just (ContentDisposition "form-data" ps) ->
                  (fromMaybe "" $ lookup "name" ps,
                   Input { inputValue = b,
                           inputFilename = lookup "filename" ps,
                           inputContentType = ctype })
              _ -> ("ERROR",simpleInput "ERROR") -- FIXME: report error
    where ctype = fromMaybe defaultInputType (getContentType hs)


simpleInput :: String -> Input
simpleInput v
    = Input { inputValue = L.pack v
            , inputFilename = Nothing
            , inputContentType = defaultInputType
            }

-- | The default content-type for variables.
defaultInputType :: ContentType
defaultInputType = ContentType "text" "plain" [] -- FIXME: use some default encoding?



{-
-- | Get the path from a Request.
reqPath :: Request -> String
reqPath = SURI.path . rqURI
-}

-- | Get the path components from a String.
pathEls :: String -> [String]
pathEls = (drop 1) . map SURI.unEscape . splitList '/' 
--    filter (not.null) $ map URI.unEscapeString $ splitList '/' path 

{-
-- | Get the path components from the Request.
getPath :: Request -> [String]
getPath req= pathEls $ reqPath req
-}
-- | Like 'Read' except Strings and Chars not quoted.
class (Read a)=>ReadString a where readString::String->a; readString =read 

instance ReadString Int 
instance ReadString Double 
instance ReadString Float 
instance ReadString SURI.SURI where readString = read . show
instance ReadString [Char] where readString=id
instance ReadString Char where 
    readString s= if length t==1 then head t else read t where t=trim s 

{-
-- | Read the named field from the request and if it fails use the
--   given default value.
look :: (Read a) => Request -> String -> a -> a
look msg name other = maybe other id $ lookup name m >>= mbRead
    where  m = bodyToList msg

-- | Get the named field from the request with a maximum length.
--   If the field is not defined return the empty String.
lookS :: Int -> Request -> String -> String
lookS maxLength msg name = take maxLength $ fromMaybe "" $ lookup name m
    where  m = bodyToList msg
--           ml = if maxLength < 0 then 2^32 else maxLength

-- | Read a named field from the request. May fail if the field doens't exist.
lookM :: Monad m => Request -> String -> m String
lookM = lookMb return


lookMb :: (Monad m) => (String -> m b) -> Request -> String -> m b
lookMb p msg name = M.lookup name m >>= p
    where  m = bodyToMap msg

-- | Read the named field from the request.
--lookMbRead :: (Read a,Monad m) => Request -> String -> m a
lookMbRead msg = maybeM . lookMb mbRead msg


lookInput :: Monad m => Request -> String -> m Input
lookInput req name
    = M.lookup name (bodyToMap' req)

-- | Read a value from a String.
mbRead :: (Read a) => String -> Maybe a
mbRead = msum . map (Just . fst) . readsPrec 5 
-}

