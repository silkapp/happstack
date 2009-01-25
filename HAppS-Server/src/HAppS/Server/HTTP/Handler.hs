{-# OPTIONS -fglasgow-exts -cpp #-}
module HAppS.Server.HTTP.Handler(request-- version,required
  ,parseResponse,putRequest
-- ,unchunkBody,val,testChunk,pack
) where
--    ,fsepC,crlfC,pversion
import Control.Exception as E
import Control.Monad
import Data.List(elemIndex, unfoldr, foldl')
import Data.Char(toLower)
import Data.Maybe ( fromMaybe, fromJust, isJust, isNothing )
import qualified Data.List as List
import qualified Data.ByteString.Char8 as P
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M
import System.IO
import Numeric
import Data.Int (Int64)
import HAppS.Server.Cookie
import HAppS.Server.HTTP.Clock
import HAppS.Server.HTTP.LazyLiner
import HAppS.Server.HTTP.Types
import HAppS.Server.HTTP.Multipart
import HAppS.Server.HTTP.RFC822Headers
import HAppS.Server.MessageWrap
import HAppS.Server.SURI(SURI(..),path,query)
import HAppS.Server.SURI.ParseURI
import HAppS.Util.ByteStringCompat
import HAppS.Util.TimeOut

import System.Log.Logger hiding (debugM)

logMH :: String -> IO ()
logMH = logM "HAppS.Server.HTTP.Handler" DEBUG

request :: Conf -> Handle -> Host -> (Request -> IO Response) -> IO ()
request conf h host handler = rloop conf h host handler =<< L.hGetContents h

required :: String -> Maybe a -> Either String a
required err Nothing  = Left err
required _   (Just a) = Right a

transferEncodingC :: [Char]
transferEncodingC = "transfer-encoding"
rloop :: t
         -> Handle
         -> Host
         -> (Request -> IO Response)
         -> L.ByteString
         -> IO ()
rloop conf h host handler inputStr
    | L.null inputStr = return ()
    | otherwise
    = join $ withTimeOut (30 * second) $
      do let parseRequest
                 = do (topStr, restStr) <- required "failed to separate request" $ splitAtEmptyLine inputStr
                      (rql, headerStr) <- required "failed to separate headers/body" $ splitAtCRLF topStr
                      let (m,u,v) = requestLine rql
                      headers' <- parseHeaders "host" (L.unpack headerStr)
--                      headers' <- required "host" $ parseBHeaders headerStr
                      let headers = mkHeaders headers'
--                      let headers = M.fromList $ headers'
                      let contentLength = fromMaybe 0 $ fmap fst (P.readInt =<< getHeaderUnsafe contentlengthC headers)
                      (body, nextRequest) <- case () of
                          () | contentLength < 0               -> fail "negative content-length"
                             | isJust $ getHeader transferEncodingC headers ->
                                 return $ consumeChunks restStr
                             | otherwise                       -> return (L.splitAt (fromIntegral contentLength) restStr)
                      let cookies = [ (cookieName c, c) | cl <- fromMaybe [] (fmap getCookies (getHeader "Cookie" headers)), c <- cl ] -- Ugle
                          rqTmp = Request m (pathEls (path u)) (path u) (query u) 
                                  [] cookies v headers (Body body) host
                          rq = rqTmp{rqInputs = queryInput u ++ bodyInput rqTmp}
                      return (rq, nextRequest)
         case parseRequest of
           Left err -> error $ "failed to parse HTTP request: " ++ err
           Right (req, rest)
               -> return $ -- logMH (show req) >>
                  do let ioseq act = act >>= \x -> x `seq` return x
                     res <- ioseq (handler req) `E.catch` \(e::E.EXCEPTION_TYPE) -> return $ result 500 $ "Server error: " ++ show e
                     putAugmentedResult h req res
                     when (continueHTTP req res) $ rloop conf h host handler rest

parseResponse :: L.ByteString -> Either String Response
parseResponse inputStr =
    do (topStr,restStr) <- required "failed to separate response" $ 
                           splitAtEmptyLine inputStr
       (rsl,headerStr) <- required "failed to separate headers/body" $
                          splitAtCRLF topStr
       let (v,code) = responseLine rsl
       headers' <- parseHeaders "host" (L.unpack headerStr)
       let headers = mkHeaders headers'
       let mbCL = fmap fst (B.readInt =<< getHeader "content-length" headers)
           rsFlags = RsFlags $ not $ isJust mbCL
       (body,nextResp) <-
           maybe (if (isNothing $ getHeader "transfer-encoding" headers) 
                       then  return (restStr,L.pack "") 
                       else  return $ consumeChunks restStr)
                 (\cl->return (L.splitAt (fromIntegral cl) restStr))
                 mbCL
       return $ Response {rsCode=code,rsHeaders=headers,rsBody=body,rsFlags=RsFlags True,rsValidator=Nothing}
val :: String
val =  "71 \r\n\n<title> i2x.com </title>\n\n\n<H1> This is i2x.com </H1>\n\nContact <a href=\"mailto:contact20020212@i2x.com\">us.</a>\n\r\n0\r\n\r\n"

testChunk :: String -> Bool
testChunk x = x ==  (L.unpack $ fst $ consumeChunks $ L.pack x)
-- http://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html
-- note this does NOT handle extenions
consumeChunks::L.ByteString->(L.ByteString,L.ByteString)
consumeChunks str = let (parts,tr,rest) = consumeChunksImpl str in (L.concat . (++ [tr]) .map snd $ parts,rest)

consumeChunksImpl :: L.ByteString -> ([(Int64, L.ByteString)], L.ByteString, L.ByteString)
consumeChunksImpl str
    | L.null str = ([],L.empty,str)
    | chunkLen == 0 = let (last,rest') = L.splitAt lenLine1 str
                          (tr,rest'') = getTrailer rest' 
                      in ([(0,last)],tr,rest'')
    | otherwise = ((chunkLen,part):crest,tr,rest2)
    where
      line1 = head $ lazylines str 
      lenLine1 = (L.length line1) + 1 -- endchar
      chunkLen = (fst $ head $ readHex $ L.unpack line1)
      len = chunkLen + lenLine1 + 2
      (part,rest) = L.splitAt len str
      (crest,tr,rest2) = consumeChunksImpl rest
      getTrailer str = L.splitAt index str
          where index | crlfLC `L.isPrefixOf` str = 2
                      | otherwise = let iscrlf = L.zipWith (\a b -> a == '\r' && b == '\n') str . L.tail $ str
                                        Just i = elemIndex True $ zipWith (&&) iscrlf (tail (tail iscrlf))
                                    in fromIntegral $ i+4
chunk :: Int64 -> L.ByteString -> L.ByteString
chunk n = L.concat . concatMap (\c -> [L.pack . flip showHex "" . L.length $ c,crlfLC,c,crlfLC]) . (++ [L.empty]) . splits n
    where 
      splits n = unfoldr (\xs -> if L.null xs then Nothing else Just (L.splitAt n xs))

unchunk :: L.ByteString -> (L.ByteString, L.ByteString -- the trailer part, unparsed, plus the final \r\n
                           ,L.ByteString)
unchunk bs = let (parts,tr,rest) = consumeChunksImpl bs
             in (L.concat . map clean $ parts,tr, rest)
    where clean (sz,bs) = L.take sz . L.drop 1 . snd . L.break (== '\n') $ bs


crlfLC :: L.ByteString
crlfLC = L.pack "\r\n"

-- Properly lazy version of 'lines' for lazy bytestrings
lazylines           :: L.ByteString -> [L.ByteString]
lazylines s
    | L.null s  = []
    | otherwise =
        let (l,s') = L.break ((==) '\n') s
        in l : if L.null s' then []
                            else lazylines (L.tail s')                      
                            



{-
presult :: Handle -> IO Result
presult h = do
    liner <- newLinerHandle h

    rls@(rql:hrl) <- headerLines liner
    let c = responseLine rql
        hh = combineHeaders $ headers hrl []

    let mread k = return (fmap fst (P.readInt =<< M.lookup k hh))
    cl   <- mread contentlengthC
    body <- case cl of
              Nothing               -> fmap toChunks $ getRest liner
              Just c | c < 0        -> fail "Negative content-length"
                     | otherwise    -> fmap toChunks $ getBytes liner c
    return $ Result c hh nullRsFlags body
-}

headers :: [B.ByteString]
           -> [(B.ByteString, B.ByteString)]
           -> [(B.ByteString, B.ByteString)]
headers []          acc = acc
headers (line:rest) acc =
  let space = let x = P.head line in x == ' ' || x == '\t' in
  case () of
    _ | space && null acc -> error "Continuation header as first header"
      | space             -> let ((k,v):r) = acc in headers rest ((k,P.append v line):r)
      | otherwise         -> let (k,raw) = breakChar ':' line
                                 v       = dropSpaceEnd $ dropSpace $ P.tail raw
                                 in headers rest ((k,v):acc)
  
requestLine :: L.ByteString -> (Method, SURI, Version)
requestLine l = case P.words ((P.concat . L.toChunks) l) of
                  [rq,uri,ver] -> (method rq, SURI $ parseURIRef uri, version ver)
                  [rq,uri] -> (method rq, SURI $ parseURIRef uri,Version 0 9)

--responseLine l = case P.words l of (v:c:_) -> version v `seq` fst (fromJust (P.readInt c))

responseLine :: L.ByteString -> (B.ByteString, Int)
responseLine l = case B.words ((B.concat . L.toChunks) l) of 
                   (v:c:_) -> version v `seq` (v,fst (fromJust (B.readInt c)))


method :: B.ByteString -> Method
method r = fj $ lookup r mtable
    where fj (Just x) = x
          fj Nothing  = error "invalid request method"
          mtable = [(P.pack "GET",     GET),
                    (P.pack "HEAD",    HEAD),
                    (P.pack "POST",    POST),
                    (P.pack "PUT",     PUT),
                    (P.pack "DELETE",  DELETE),
                    (P.pack "TRACE",   TRACE),
                    (P.pack "OPTIONS", OPTIONS),
                    (P.pack "CONNECT", CONNECT)]


combineHeaders :: [(P.ByteString,P.ByteString)] -> M.Map P.ByteString P.ByteString
combineHeaders = foldl' w M.empty
    where w m (k,v) = M.insertWith (\n o -> P.concat [o, P.pack ", ", n])
                                   (P.map toLower k) v m

-- Result side

staticHeaders :: Headers
staticHeaders =
    foldr (uncurry setHeaderBS) (mkHeaders [])
    [ (serverC, happsC), (contentTypeC, textHtmlC) ]

putAugmentedResult :: Handle -> Request -> Response -> IO ()
putAugmentedResult h req res = do
  let ph (HeaderPair k vs) = map (\v -> P.concat [k, fsepC, v, crlfC]) vs
  raw <- getApproximateTime
  let cl = L.length $ rsBody res
  let put x = P.hPut h x
  -- TODO: Hoist static headers to the toplevel.
  let stdHeaders = staticHeaders `M.union`
                   M.fromList ( [ (dateCLower,       HeaderPair dateC [raw])
                                , (connectionCLower, HeaderPair connectionC [if continueHTTP req res then keepAliveC else closeC])
                                ] ++ if rsfContentLength (rsFlags res)
                                     then [(contentlengthC, HeaderPair contentLengthC [P.pack (show cl)])]
                                     else [] )
      allHeaders = rsHeaders res `M.union` stdHeaders  -- 'union' prefers 'headers res' when duplicate keys are encountered.

  mapM_ put $ concat
    [ (pversion $ rqVersion req)          -- Print HTTP version
    , [responseMessage $ rsCode res]      -- Print responseCode
    , concatMap ph (M.elems allHeaders)   -- Print all headers
    , [crlfC]
    ]
  when (rqMethod req /= HEAD) $ L.hPut h $ rsBody res
--  logMH "Flushing connection"
  hFlush h


putRequest :: Handle -> Request -> IO ()
putRequest h rq = do 
    let put x = B.hPut h x
        ph (HeaderPair k vs) = map (\v -> B.concat [k, fsepC, v, crlfC]) vs
        sp = [B.pack " "]
    mapM_ put $ concat
      [[B.pack $ show $ rqMethod rq],sp
      ,[B.pack $ rqURL rq],sp
      ,(pversion $ rqVersion rq), [crlfC]
      ,concatMap ph (M.elems $ rqHeaders rq)
      ,[crlfC]
      ]
    let Body body = rqBody rq
    L.hPut h  body
    hFlush h



-- Version

pversion :: Version -> [B.ByteString]
pversion (Version 1 1) = [http11]
pversion (Version 1 0) = [http10]
pversion (Version x y) = [P.pack "HTTP/", P.pack (show x), P.pack ".", P.pack (show y)]

version :: B.ByteString -> Version
version x | x == http09 = Version 0 9
          | x == http10 = Version 1 0
          | x == http11 = Version 1 1
          | otherwise   = error "Invalid HTTP version"

http09 :: B.ByteString
http09 = P.pack "HTTP/0.9"
http10 :: B.ByteString
http10 = P.pack "HTTP/1.0"
http11 :: B.ByteString
http11 = P.pack "HTTP/1.1"

-- Constants

connectionC :: B.ByteString
connectionC      = P.pack "Connection"
connectionCLower :: B.ByteString
connectionCLower = P.map toLower connectionC
closeC :: B.ByteString
closeC           = P.pack "close"
keepAliveC :: B.ByteString
keepAliveC       = P.pack "Keep-Alive"
--connectionCloseC = P.pack "Connection: close\r\n"
--connectionKeepC  = P.pack "Connection: keep-alive\r\n"
crlfC :: B.ByteString
crlfC            = P.pack "\r\n"
fsepC :: B.ByteString
fsepC            = P.pack ": "
contentTypeC :: B.ByteString
contentTypeC     = P.pack "Content-Type"
contentLengthC :: B.ByteString
contentLengthC   = P.pack "Content-Length"
contentlengthC :: B.ByteString
contentlengthC   = P.pack "content-length"
dateC :: B.ByteString
dateC            = P.pack "Date"
dateCLower :: B.ByteString
dateCLower       = P.map toLower dateC
serverC :: B.ByteString
serverC          = P.pack "Server"
happsC :: B.ByteString
happsC           = P.pack "HAppS/0.9.2"
textHtmlC :: B.ByteString
textHtmlC        = P.pack "text/html; charset=utf-8"

-- Response code names

responseMessage :: (Num t) => t -> B.ByteString
responseMessage 100 = P.pack " 100 Continue\r\n"
responseMessage 101 = P.pack " 101 Switching Protocols\r\n"
responseMessage 200 = P.pack " 200 OK\r\n"
responseMessage 201 = P.pack " 201 Created\r\n"
responseMessage 202 = P.pack " 202 Accepted\r\n"
responseMessage 203 = P.pack " 203 Non-Authoritative Information\r\n"
responseMessage 204 = P.pack " 204 No Content\r\n"
responseMessage 205 = P.pack " 205 Reset Content\r\n"
responseMessage 206 = P.pack " 206 Partial Content\r\n"
responseMessage 300 = P.pack " 300 Multiple Choices\r\n"
responseMessage 301 = P.pack " 301 Moved Permanently\r\n"
responseMessage 302 = P.pack " 302 Found\r\n"
responseMessage 303 = P.pack " 303 See Other\r\n"
responseMessage 304 = P.pack " 304 Not Modified\r\n"
responseMessage 305 = P.pack " 305 Use Proxy\r\n"
responseMessage 307 = P.pack " 307 Temporary Redirect\r\n"
responseMessage 400 = P.pack " 400 Bad Request\r\n"
responseMessage 401 = P.pack " 401 Unauthorized\r\n"
responseMessage 402 = P.pack " 402 Payment Required\r\n"
responseMessage 403 = P.pack " 403 Forbidden\r\n"
responseMessage 404 = P.pack " 404 Not Found\r\n"
responseMessage 405 = P.pack " 405 Method Not Allowed\r\n"
responseMessage 406 = P.pack " 406 Not Acceptable\r\n"
responseMessage 407 = P.pack " 407 Proxy Authentication Required\r\n"
responseMessage 408 = P.pack " 408 Request Time-out\r\n"
responseMessage 409 = P.pack " 409 Conflict\r\n"
responseMessage 410 = P.pack " 410 Gone\r\n"
responseMessage 411 = P.pack " 411 Length Required\r\n"
responseMessage 412 = P.pack " 412 Precondition Failed\r\n"
responseMessage 413 = P.pack " 413 Request Entity Too Large\r\n"
responseMessage 414 = P.pack " 414 Request-URI Too Large\r\n"
responseMessage 415 = P.pack " 415 Unsupported Media Type\r\n"
responseMessage 416 = P.pack " 416 Requested range not satisfiable\r\n"
responseMessage 417 = P.pack " 417 Expectation Failed\r\n"
responseMessage 500 = P.pack " 500 Internal Server Error\r\n"
responseMessage 501 = P.pack " 501 Not Implemented\r\n"
responseMessage 502 = P.pack " 502 Bad Gateway\r\n"
responseMessage 503 = P.pack " 503 Service Unavailable\r\n"
responseMessage 504 = P.pack " 504 Gateway Time-out\r\n"
responseMessage 505 = P.pack " 505 HTTP Version not supported\r\n"
responseMessage x   = P.pack (show x ++ "\r\n")

