module HAppS.Server.HTTP.Client where


import HAppS.Server.HTTP.Handler
import HAppS.Server.HTTP.Types
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as L 
--import HAppS.Server.AlternativeHTTP

import System.IO
import qualified Data.ByteString.Char8 as B 
import Network

getResponse rq = withSocketsDo $ do
  let (hostName,port) = span (/=':') $ fromJust $ fmap B.unpack $ getHeader "host" rq 
      portInt = if null port then 80 else read $ tail port
      portId = PortNumber $ toEnum $ portInt
  h <- connectTo hostName portId 
  hSetBuffering h NoBuffering
  --print (hostName,portInt)
  --putRequest stdout rq
  --hFlush stdout

  putRequest h rq
  hFlush h

  inputStr <- L.hGetContents h
  --print $ L.take 200 inputStr
  return $ parseResponse inputStr

unproxify rq = rq {rqPaths = tail $ rqPaths rq,
                   rqHeaders = 
                       forwardedFor $ forwardedHost $ 
                       setHeader "host" (head $ rqPaths rq) $
                   rqHeaders rq}
  where
  appendInfo hdr val x = setHeader hdr (csv val $
                                        maybe "" B.unpack $
                                        getHeader hdr rq) x
  forwardedFor = appendInfo "X-Forwarded-For" (fst $ rqPeer rq)
  forwardedHost = appendInfo "X-Forwarded-Host" 
                  (B.unpack $ fromJust $ getHeader "host" rq)
  --forwardedServer = appendInfo "X-Forwarded-Server" 
  --                  how do we get server hostname? do we want this?
  csv v "" = v
  csv v x = x++", " ++ v

unrproxify defaultHost list rq = unproxify rq {rqPaths = host: rqPaths rq}
  where
  host::String
  host = maybe defaultHost (f .B.unpack) $
         getHeader "host" rq
  f = maybe defaultHost id . flip lookup list



{--      setHeader forwardedForC  (csv  $ 
                 maybe "" B.unpack $
                 getHeader forwardedForC rq) x--}
{--    where cmd = "curl"

          args =["-L","-D","-"]++postFlag++[url]
          url = tail $ rqURL rq
          Body body = rqBody rq
          meth = rqMethod rq
          postFlag::[String]
          postFlag = if meth == GET then [] else ["--data-binary",L.unpack body]
          ctype = maybe [] ((\c->["-H","content-type: "++c]) . B.unpack) $ 
                  getHeader "content-type" rq
          io = do
            (hIn,hOut,hErr,pi) <- runInteractiveProcess cmd args Nothing Nothing
            inputStr <- L.hGetContents hOut
            print inputStr
            return $ parseResponse inputStr
   --}         

          -- -H "host: abc" sends a header
          -- and we get back the header with
          -- post content-type and return content-type


