{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fth #-}
module HAppS.State.Logger
    (LogFormat(..), Logger, LogChan,
     newLogChan, addToLog
    ) where

import Control.Concurrent.Chan
import Control.Exception as E(Exception)
import Data.ByteString.Lazy.Char8(pack)
import System.IO
import System.Locale(defaultTimeLocale)
import System.Time

import HAppS.State.Saver
import HAppS.State.Types
import HAppS.Util.Concurrent(forkEver)

type Logger req  = TxContext -> req -> Exception -> IO ()
type LogChan     = Chan String

class LogFormat ty where
    logFormat :: Int -> ty -> String
    logFormat _ _ = "<LoggerFormat: not supported>"

instance LogFormat () where logFormat _ _ = "()"
instance Show a => LogFormat a where logFormat _ x = show x

newLogChan :: SaverImpl -> IO LogChan
newLogChan si = do
  ch <- newChan
  forkEver (readChan ch >>= \s -> saverAdd si (pack s) (return ()))
  return ch

addToLog :: LogFormat req => LogChan -> Logger req
addToLog ch context req exc = do
  ct <- toCalendarTime $ TOD (fromIntegral (txTime context)) 0
  writeChan ch $ concat ["================================================================================"
                        ,"\nTransaction: ", show $ txId context
                        ,"\nTx time:     ", show $ txTime context, formatCalendarTime defaultTimeLocale " = %Y-%m-%d %T %Z" ct
                        ,"\nError:       ", show exc
                        ,"\nRequest:\n",    logFormat 5 $ req
                        ,"\n================================================================================\n"
                        ]
