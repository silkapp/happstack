{-# OPTIONS -fglasgow-exts #-}
module HAppS.Agents.MailSender
{-    (MailState, State, queueMessage
    ) -} where

{-
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.QSem
import Control.Exception
import qualified Control.Monad.State as S
--import Data.Binary
import Data.Typeable
import qualified HAppS.DBMS.RSMap as M
import HAppS.MACID
import HAppS.MACID.Logger
import HAppS.Protocols.SMTP
import HAppS.Util.Concurrent
import HAppS.Util.Common
import HAppS.Util.StdMain.StartState
import System.IO.Unsafe
import System.Random
import System.Log.Logger

logMM = logM "HAppS.Agents.MailSender"

data QueueItem    = QI [EpochSeconds] (Envelope String)
                  deriving(Read,Show,Typeable)
type QMap = M.Map Key QueueItem
type Key  = (EpochSeconds, Integer)
data MailState = State { queue :: QMap,
                         ci    :: Integer
                       } deriving(Read,Show,Typeable)

type State = MailState

instance StartState State where
    startStateM = return startState
    runPart     = fmap (:[]) . mailSender
instance LogFormat Request where
    logFormat _ = show

-- | Initial empty state
startState :: State
startState = State M.empty 1

--instance EventRelation Request ()

data Request  = Run | Sent [Key] deriving(Read,Show,Typeable)

instance Serialize Request where
    typeString    _ = "Happs.Agents.MailSender.Request"
    encodeStringM   = defaultEncodeStringM
    decodeStringM   = defaultDecodeStringM
instance Serialize State where
    typeString    _ = "Happs.Agents.MailSender.State"
    encodeStringM   = defaultEncodeStringM
    decodeStringM   = defaultDecodeStringM


addQIs :: [(Key,QueueItem)] -> State -> State
addQIs qs st = st { queue = qfun $ queue st }
    where qfun = foldl (.) id [M.insert (e,idx) (QI es v) | ((_,idx),QI (e:es) v) <- qs ]


-- | Send a message.
queueMessage :: Envelope String -> Ev MailState ev ()
queueMessage ez = do
  t0 <- getTime
  st <- S.get
  let key    = (t0, ci st)
  let delays = [ t0 + (60*x) | x <- [20,60,720,1440]]
  let qi     = QI delays ez
  S.put $ st { queue = M.insert key qi $ queue st, ci = (ci st + 1) }

maxConcurrentMails :: Int
maxConcurrentMails = 40

mailSender :: (st -> State) -> (State -> st -> st) -> Handler st
mailSender ifun ufun = undefined {-$ do --IoH $ do
    mv <- newEmptyMVar
    -- Limit the number of mails we'll try to send concurrently.
    -- This is way ugly and not really necessary. Concurrent side-effects
    -- are already limited to a decent number.
    nMails <- atomically $ newRefSTM maxConcurrentMails
    let ior = do forkIO $ let loop = sleep 10 >> putMVar mv Run >> loop in loop
                 return (takeMVar mv, \_ -> return ())
    return $ error "mailSender" -- ior :=> (getEvent >>= wh nMails ifun ufun mv)
-}
-- 
trySend (QI ts env) = withSocket $ smtpTry (send env) f1 f2 ok
    where ok _ = do logMM NOTICE ("MailServer: Message delivery ok: "++showMsg env)
                    return True
          f1 e = do logMM WARNING ("MailServer: Error sending mail (RETRY): "++show e)
                    return False
          f2 e = if smtpErrorMajorCode e == 4 && not (null ts)
                    then do logMM WARNING ("MailServer: Temporary failure: (RETRY): "++show e)
                            return False
                    else do logMM ERROR ("MailServer: Message delivery failure (DROP): "++show e)
                            print env
                            return True

{-
pretty :: Ev State ev String
pretty = do st <- S.get
            return $ unlines (["Queue",""]++[ show k++"\t"++showMsg m | (k,QI _ m) <- M.toList (queue st) ])
-}

showMsg m = unwords (show (sender m) : "->" : map show (recipients m))

wh :: MutVar Int -> (outer -> MailState) -> (MailState -> outer -> outer) -> MVar Request -> Request -> Ev outer Request ()
wh _ i u _  (Sent ms) = localState i u $ S.modify $ \st -> st { queue = foldr M.delete (queue st) ms }
wh nMailsRef i u mv (Run)     = do
    ep <- getTime
    st <- S.gets i
    let (c,r) = M.split (ep,-1) (queue st)
        cs    = M.toList c
    S.when (not (M.null c)) $ do
        nMails <- readRef nMailsRef
        let (cs', r') = splitAt nMails cs
        writeRef nMailsRef (nMails - length cs')
        S.modify $ u (addQIs cs' st { queue = M.fromList r' `M.union` r })
        mapM_ (trySendQI nMailsRef mv) cs'


trySendQI :: MutVar Int -> MVar Request -> (Key,QueueItem) -> Ev st any ()
trySendQI nMailsRef mv ((_,idx),qi@(QI ts _))
    = addSideEffect 40 (effect `finally` modifyRef succ nMailsRef)
    where modifyRef fn ref = atomically (writeRefSTM ref =<< fmap fn (readRefSTM ref))
          effect = do sleep =<< randomRIO (0,10)
                      res <- trySend qi
                      if res && not (null ts) then putMVar mv $ Sent [(head ts,idx)] else return ()


{-# NOINLINE max_simultaneous_sockets #-}
max_simultaneous_sockets = unsafePerformIO $ newQSem 64

withSocket = bracket_ (waitQSem max_simultaneous_sockets) (signalQSem max_simultaneous_sockets)

-}
