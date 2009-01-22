{-# LANGUAGE TemplateHaskell, FlexibleInstances , UndecidableInstances,
             DeriveDataTypeable, MultiParamTypeClasses, CPP, ScopedTypeVariables  #-}
-- | Implement XSLT transformations using xsltproc
module HAppS.Server.XSLT
    (xsltFile, xsltString, xsltElem, xsltFPS, xsltFPSIO, XSLPath,
     xsltproc,saxon,procFPSIO,procLBSIO,XSLTCommand,XSLTCmd
    ) where


import System.Log.Logger

import HAppS.Server.MinHaXML
import HAppS.Util.Common(runCommand)
import Control.Exception(bracket,try,EXCEPTION_TYPE)
import qualified Data.ByteString.Char8 as P
import qualified Data.ByteString.Lazy.Char8 as L
import System.Directory(removeFile)
import System.Environment(getEnv)
import System.IO
import System.IO.Unsafe(unsafePerformIO)
import Text.XML.HaXml.Verbatim(verbatim)
import HAppS.Data hiding (Element)

logMX = logM "HAppS.Server.XSLT"

type XSLPath = FilePath
#ifndef __HADDOCK__
$(deriveAll [''Show,''Read,''Default, ''Eq, ''Ord]
   [d|
       data XSLTCmd = XSLTProc | Saxon 
    |]
   )
#endif
xsltCmd XSLTProc = xsltproc'
xsltCmd Saxon = saxon'

xsltElem :: XSLPath -> Element -> String
xsltElem xsl = xsltString xsl . verbatim


procLBSIO xsltproc' xsl inp = 
    withTempFile "happs-src.xml" $ \sfp sh -> do
    withTempFile "happs-dst.xml" $ \dfp dh -> do
    let xsltproc = xsltCmd xsltproc'
    L.hPut sh inp
    hClose sh
    hClose dh
    xsltFileEx xsltproc xsl sfp dfp
    s <- L.readFile dfp
    logMX DEBUG (">>> XSLT: result: "++ show s)
    return s


procFPSIO xsltproc xsl inp = 
    withTempFile "happs-src.xml" $ \sfp sh -> do
    withTempFile "happs-dst.xml" $ \dfp dh -> do
    mapM_ (P.hPut sh) inp
    hClose sh
    hClose dh
    xsltFileEx xsltproc xsl sfp dfp
    s <- P.readFile dfp
    logMX DEBUG (">>> XSLT: result: "++ show s)
    return [s]

xsltFPS :: XSLPath -> [P.ByteString] -> [P.ByteString]
xsltFPS xsl inp = unsafePerformIO $ xsltFPSIO xsl inp

xsltFPSIO xsl inp = 
    withTempFile "happs-src.xml" $ \sfp sh -> do
    withTempFile "happs-dst.xml" $ \dfp dh -> do
    mapM_ (P.hPut sh) inp
    hClose sh
    hClose dh
    xsltFile xsl sfp dfp
    s <- P.readFile dfp
    logMX DEBUG (">>> XSLT: result: "++ show s)
    return [s]

xsltString :: XSLPath -> String -> String
xsltString xsl inp = unsafePerformIO $
    withTempFile "happs-src.xml" $ \sfp sh -> do
    withTempFile "happs-dst.xml" $ \dfp dh -> do
    hPutStr sh inp
    hClose sh
    hClose dh
    xsltFile xsl sfp dfp
    s <- readFileStrict dfp
    logMX DEBUG (">>> XSLT: result: "++ show s)
    return s

-- | Note that the xsl file must have .xsl suffix.
xsltFile :: XSLPath -> FilePath -> FilePath -> IO ()
xsltFile = xsltFileEx xsltproc'

-- | Use @xsltproc@ to transform XML.
xsltproc' :: XSLTCommand
xsltproc' dst xsl src = ("xsltproc",["-o",dst,xsl,src])
xsltproc = XSLTProc

-- | Use @saxon@ to transform XML.
saxon = Saxon
saxon' :: XSLTCommand
saxon' dst xsl src = ("java -classpath /usr/share/java/saxon.jar",
                     ["com.icl.saxon.StyleSheet"
                     ,"-o",dst,src,xsl])
                        
type XSLTCommand = XSLPath -> FilePath -> FilePath -> (FilePath,[String])
xsltFileEx   :: XSLTCommand -> XSLPath -> FilePath -> FilePath -> IO ()
xsltFileEx xsltproc xsl src dst = do
    let msg = (">>> XSLT: Starting xsltproc " ++ unwords ["-o",dst,xsl,src])
    logMX DEBUG msg
    uncurry runCommand $ xsltproc dst xsl src
    logMX DEBUG (">>> XSLT: xsltproc done")

-- Utilities

withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile str hand = bracket (openTempFile tempDir str) (removeFile . fst) (uncurry hand)

readFileStrict :: FilePath -> IO String
readFileStrict fp = do
    let fseqM [] = return [] 
        fseqM xs = last xs `seq` return xs
    fseqM =<< readFile fp

{-
hGetContentsStrict :: Handle -> IO String
hGetContentsStrict h = flip finally (hClose h) $ do
    r <- hGetContents h
    let fseq [] = []; fseq xs = last xs `seq` xs
    return $! fseq r
-}

{-# NOINLINE tempDir #-}
tempDir :: FilePath
tempDir = unsafePerformIO $ tryAny [getEnv "TEMP",getEnv "TMP"] err
    where err = return "/tmp"

tryAny :: [IO a] -> IO a -> IO a
tryAny [] c     = c
tryAny (x:xs) c = either (\(_::EXCEPTION_TYPE) -> tryAny xs c) return =<< try x
