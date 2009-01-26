{-# OPTIONS -fglasgow-exts -fth -fallow-undecidable-instances -cpp #-}

{--
the main SMTP funcitions are
  * serve: listen on a port and deliver based on function passed
  * serveRelay: receive locally and attempt to send
  * sendEnvs: attempt to send and list of envelopes using the correct mxserver for each recip
  * retrySend: stores envelopes in the filesystem until sent or applies dsn function to error

Todo
  * support ehlo on client 
  * make receive handle smtp-auth so not operating an open relay
  * count volume of mail relayed


  * DONE support locking so multiple processes can send mail form the same default directory
  * DONE rertySend so we can restart and handle undelivered messages
  * DONE add received header in receive code
  * DONE fix sendEnvs to sort the envs by the recipient domain
  * DONE group by mx record, 
  * DONE send to the mxrecord  or try next mx record
  * DONE recieve a set of envelopes from client and relay batch to wherever

--}

module HAppS.Network.SMTP (receive,sendEnvs,receiveAndRelay,
                           Envelope(..),(@@),Address(..)) where

-- module HAppS.Server.SMTP2 where

import Network
import System.IO(Handle,hClose)
import System.Environment(getEnv)
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.OldException as E
import System.IO
import Data.Typeable
import System.Directory
import System.FilePath
import System.Time
import Data.List
import Data.Char
import HAppS.Network.DNS
import System.Cmd (rawSystem)
import System.Random

sendEnvs = smtpSend
receive=serve
serve = smtpServe
receiveAndRelay = smtpRelay


data Envelope = Envelope {mail_from::MAIL_FROM,rcpt_tos::[Addr],contents::Contents} 
                deriving (Eq,Ord,Read,Show)
recips = rcpt_tos
newtype HELO = HELO Domain deriving (Eq,Ord,Read,Show)
newtype MAIL_FROM = MAIL_FROM Addr deriving (Eq,Ord,Read,Show)
type Contents = String
type Domain = String
type Addr= Address
data Address = Addr UserId Domain deriving (Eq,Ord,Read,Show)
addrDomain (Addr _ d)= d
type UserId = String

(@@) =Addr
exampleAddr = "bob" @@ "example.com"

listen portNum hand = forkIO $ do
  s <- listenOn $ PortNumber $ toEnum portNum
  let work (h,hn,p) = do hand h (hn,fromIntegral p)
                         hClose h
      fork = forkIO
      msg = "\n Error bad news on socket\n"
      loop = try (accept s) >>= flip either  (fork . work ) 
             (\err-> (fork $ hPutStr stderr ((show err)++msg)))  
             >> loop
  loop `finally` sClose s

ltrim = dropWhile isSpace
rtrim = reverse.ltrim.reverse
trim=rtrim.ltrim

type PortNum = Int
type DeliverFun = ((Domain,Envelope) -> IO ())
smtpServe::HostName->PortNum->DeliverFun -> IO ThreadId
smtpServe hostname port deliver = listen port $ smtpServe' hostname deliver
               
smtpServe' hostName deliver h (peer,pp) = do 
  hPutLine h $ "220 "++hostName++" ESMTP Server"
  ready
  where
  getLine = return . rtrim =<< (\line->when debug (print line)>>return line) =<< hGetLine h 
  putLine line = hPutLine h line >> (when debug $ print line)
  ok addr entity = putLine $ "250 2.1.2 <"++(show addr)++">... "++entity++" ok"
  ready = do
    line <- getLine 
    case take 4 line of
      "QUIT" -> quit
      "RSET" -> rset
      "HELO" -> helo $ drop 5 line
      "EHLO" -> helo $ drop 5 line
      "NOOP" -> noop >> ready
      otherwise -> error $ "ready " ++  take 4 line
  unBracket = tail.reverse.tail.reverse.trim
  rset = putLine "250 2.0.0 Reset state" >> ready
  noop = putLine "250 2.0.0 OK" 
  quit = putLine $ "221 "++hostName++" closing connection"
  toAddr s = let (uid,domain) = span (/='@') s in Addr uid $ tail domain
  helo domain = do
    putLine $ "250 "++hostName++" Hello "++domain -- need "-" after 250 if multiline
    -- if ehlo then handle auth cramd 
    -- putLine "250-AUTH CRAM-MD5"
    -- now if we require auth we do it here.  then do heloImpl otherwise heloimpl
    -- the envelope doesn't need auth information
    heloImpl domain
  heloImpl domain = do
    line <- getLine
    case take 4 line of
      "MAIL" -> mail_from domain $ toAddr $ unBracket $ drop 10 line
      "QUIT" -> quit
      "NOOP" -> noop >> heloImpl domain 
      "RSET" -> rset
      otherwise -> error $ "helo " ++ take 4 line
  mail_from domain addr = do
    ok addr "Sender"
    getRcpts domain addr []
  getRcpts domain addr rcpts = do
    line <- getLine
    case take 4 line of 
      "RCPT" -> do let rcpt = toAddr $ unBracket $ drop 8 line 
                   ok rcpt "Recipient"
                   getRcpts domain addr (rcpt:rcpts)
      "DATA" -> do putLine "354 Enter mail, end with \".\" on a line by itself" 
                   handle_data domain addr rcpts []
      "QUIT" -> quit
      "NOOP" -> noop >> getRcpts domain addr rcpts
      "RSET" -> rset
      otherwise -> error $ "rcpts " ++ line
  handle_data domain addr rcpts lines = do
      line <- getLine
      let undot line = if length line>1 && head line == '.' then tail line else line 
      if line /= "." then handle_data domain addr rcpts (undot line:lines) else do
      t <- getClockTime                                                                       
      let env = (Envelope (MAIL_FROM addr) rcpts $ 
                 addReceived t domain $ reverse lines)
      deliver (domain,env)
      putLine "250 2.0.0 Message accepted for delivery" 
      heloImpl domain
  addReceived t domain lines = 
    let rcvd = concat ["Received: from ",domain," (",peer,") by ",hostName,
                   " (HAppS-SMTP); ",show t] in
    unlines $ rcvd:lines
  unlines = intercalate "\r\n" 
{-- 
Received: from mx-out.facebook.com (out011.sctm.tfbnw.net [204.15.20.138]) by 
    mailgate.vo.com (Postfix) with ESMTP id EEF761A5BB4 for <alex@alexjacobson.com>; 
    Wed, 30 Jan 2008 22:04:29 -0500 (EST)
--}


{-- 
  sends all envs to the server
  each envelope has multiple recipients so we can handle this by just
  breaking each envelope into multiple envelopes with the same recipient domains
  sort the envelopes groups by recipient domain and reconsolidate into one list
  so we can send multiple envelopes on the same connection
--}
splitEnv::Envelope -> [Envelope]
splitEnv (Envelope f rs c) = map (\rs-> (Envelope f rs c)) $
                               groupBy (\(Addr _ d1) (Addr _ d2)->d1==d2) $ 
                               sort rs

mergeEnvs::[[Envelope]]->[[Envelope]]
mergeEnvs es = 
    groupBy (\x y->domain x == domain y) $
    sortBy (\x y->compare (domain x) (domain y)) $ 
    concat es
    where
    domain=recipDomain

recipDomain = addrDomain . head . recips 
groupEnvs::[Envelope] -> [[Envelope]]
groupEnvs = mergeEnvs . map splitEnv

{--
  * produce DNS failure/connect failure
  * handle code failures
--}

{--
  all the envelopes are to the same domain so any network error is for the group
  any error is a transient error except success or permanent failure
--}

smtpSend domain = mapM (sendMX domain) . groupEnvs 

sendMX domain group = (withMXConnection (recipDomain $ head group) 25 $ 
                      sendEnvs' domain group) 

{--
  write group to directory groupid_current_(next)
  fork to send from directory and perhaps delete from directory or update information
--}
tryFile dsn path = do
  TOD t _ <- getClockTime
  let fparts = words $ map (\x->if x=='_' then ' ' else x) groupFile
      (dir,groupFile) = splitFileName path
      [groupId,last,next] = map read fparts
      --path = concat [dir,"/",groupFile]
      nextFile = combine dir $ concat $
                 intersperse "_" $ map show [groupId,t,next']
      lockfile = path++".lock"
      ds=dropWhile (/=next-last) delays 
      (next',isLastTry) = case length ds of
                            0 -> (0,True)
                            1 -> (0,True)
                            _ -> (t+(head $ tail ds),False)
  
  when debug $ print ("trying",path)
  if (".lock" `isSuffixOf` path) || length fparts /= 3 || t < next  then return () else do
  fe <- doesFileExist path
  when debug $ print ("trying2 path,nextfile:",path,nextFile,lockfile,fe)
  lock_time <- try (readFile lockfile) >>= return . either (const 0) read
  lock_fe <- doesFileExist lockfile
  if lock_fe && (t-lock_time < 300) then return () else do

  writeFile lockfile $ show t
  (domain,group) <- return . read =<< readFile path
  let tryLater x = if isLastTry then rm path >> ( dsn domain group $ Left x)
                     else move path nextFile
      succeed x = rm path >> succeedDSN x
      succeedDSN [] = return ()
      succeedDSN x = dsn domain group $ Right x
  try (sendMX domain group) >>= either tryLater (succeed)
  rm lockfile

firstTry domain dir dsn envs = do
  let groups = groupEnvs envs
  groupFiles <- mapM mkFile groups
  print ("groupfiles",groupFiles)
  mapM (forkIO . tryFile dsn) groupFiles
  print "DONE forking"
  return ()
  where
  mkFile group = do
    TOD t m <- getClockTime
    r <- randomIO
    let groupId = (t*1000+m)*1000000+r
        fn = concat $ intersperse "_" $ map show [groupId,t-1,t-1]
        path = concat [dir,"/",fn]
    writeFile path $ show (domain,group)
    return path

type DSN_handler = Domain -> [Envelope] -> Either Exception [[(Addr,String)]] -> IO ()
noDSN _ _ _ = return ()
retrySend (domain::Domain) (dir::FilePath) (dsn::DSN_handler) = do
  createDirectoryIfMissing True dir
  let loop = getDirectoryContents dir >>= mapM (tryFile dsn . combine dir) >> 
             threadDelay oneMinute >> loop
      oneMinute = 60 * 1000 * 1000
  forkIO loop
  return $ firstTry domain dir dsn


smtpRelay hostName dir dsn port = do
  firstTry <- retrySend hostName dir dsn
  smtpServe hostName port $ firstTry . (:[]) . snd

testRelay = smtpRelay "foobar.com" "mail" noDSN 2700 
{--
smtpRelay hostname port = do
  mvar <- newEmptyMVar
  let loop = takeMVar mvar >>= send hostname >> loop
  forkIO loop
  let deliver = putMVar mvar
  --listen port $ smtpServe hostName $ deliver . (:[])
  smtpServe hostname port $ deliver . (:[]) . snd
  return ()
--}




rm fp = sys ["rm",fp]
move a b  = sys ["mv",a,b]
sys cmd@(a:xs) = -- print cmd >>
    do
    rawSystem a xs
    return ()

delays=map (60*) $ 0:15 : (take 6 $ map (\x->100*2^x)  [1..])
  

debug = False

sendEnvs' domain envs h = withSocketsDo $ do
  hSetBuffering h NoBuffering
  getLine >>= ver isReady
  results <- mapM sendEnv envs
  putLine "QUIT"
  getLine >>= ver isQuit
  return results
  where
  sendEnv env@(Envelope -- (HELO domain)
               (MAIL_FROM (Addr uid fromDomain)) rcpts content) =  
    do
    doAccept $ "HELO " ++ domain
    doAccept $ concat ["MAIL FROM:<",uid,"@",fromDomain,">"]
    results <- mapM rcpt_to rcpts
    doLine isData "DATA"
    doAccept $ concat [dot content,"\r\n."]
    reset
    return $ filter (\(x,res) -> not $ isAccept res) $ zip rcpts results
  reset = doLine isAccept "RSET"
  rcpt_to (Addr uid domain) = doAccept $ concat ["RCPT TO:<",uid,"@",domain,">"]
  getLine = do line <- hGetLine h>>= return . rtrim; when debug $ print line; return line
  putLine line = when debug (print line) >> hPutLine h line
  doLine test text = putLine text >> getLine >>= ver test
  doAccept = doLine isAccept
  failure s = do putLine "QUIT" 
                 throwDyn $ SMTPError s
  ver f x = if f x then return x else failure x
  dot msg = unlines $ 
            map (\x->if x/=[] && head x=='.' then ('.':x) else x) $ 
	    lines $ msg          
  isReady =  isPrefixOf "220" 
  isAccept x = isPrefixOf "250" x || isPrefixOf "251" x
  isData = isPrefixOf "354 "
  isQuit = isPrefixOf "221 "


data SMTPError =  SMTPError String deriving(Show,Typeable)

hPutLine h c = hPutStr h c >> hPutStr h "\r\n" >> hFlush h

testSend = smtpSend "fakeDomain" [exampleEnv,exampleEnv2]
testBadSend = smtpSend "fakeDomain" [badAddrEnv]

test p = do 
  tid <- listen p testServe 
  testClient p
  testClient p
  testClient p
  killThread tid



testServe h _ = do cs <- hGetContents h 
                   print $ take 10 cs
                   return ()

testClient p = withSocketsDo $ do
  h <- connectTo "127.0.0.1" $ PortNumber $ toEnum p
  hSetBuffering h NoBuffering
  hPutStr h "0123456789A"


exampleEnv = Envelope -- (HELO "helo.com") 
             (MAIL_FROM $ exampleAddr ) [exampleAddr] "message content\n====================="
exampleEnv2 = Envelope -- (HELO "helo2.com") 
             (MAIL_FROM $ exampleAddr ) [exampleAddr] "message content\n====================="

badAddrEnv = Envelope -- (HELO "helo.com") 
             (MAIL_FROM $ exampleAddr ) ["alex" @@ "aslkdjaslkdjasdlkjcnan1233"] 
                                            "message content\n====================="

