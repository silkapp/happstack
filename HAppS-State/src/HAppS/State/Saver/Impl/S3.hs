module HAppS.MACID.Saver.Impl.S3(S3.AccessKey,S3.SecretKey,s3Saver) where

import HAppS.MACID.Saver.Types
import HAppS.MACID.Saver.Impl.Queue
import HAppS.Protocols.S3 as S3


import Control.Concurrent
import Control.Exception        ( try )
import Control.Monad            ( when )
import qualified Data.ByteString.Char8 as P
import Data.List                ( isPrefixOf )
import Network.URI              ( URI )
import System.IO
import System.IO.Error          ( mkIOError, doesNotExistErrorType )

{-
  We can't append to files on an S3 server so we split the files over several objects.
  "events-0000001" -> "events-0000001-0"
                      "events-0000001-1"
                      and so on.
-}
s3Saver :: BucketID -> ObjectID -> AccessKey -> SecretKey -> URI -> IO SaverImpl
s3Saver bucket object akey skey uri
    = do s3 <- S3.newS3 akey skey uri
         try (S3.sendRequest_ s3 (createBucket s3 bucket)) -- Creating the bucket now is imporant
                                                        -- to avoid errors later.
         -- The files on S3 are cached for better performance.
         files <- newMVar =<< S3.listObjects s3 bucket
         chan <- newSaverQueue $ \rqss ->
                   do dat'  <- flip mapM rqss $ \(SaveRequest nVar bucket object info fin)  ->
                               modifyMVar nVar $ \n ->
                               do let newFile = object++"-"++show n
                                      request = S3.createObject s3 bucket newFile (concatMap P.unpack info)
                                  modifyMVar_ files (\fs -> return (newFile:fs))
                                  return (n+1,(request,fin))
                      let (rqsts, fins) = unzip dat'
                      S3.sendRequests s3 rqsts fins
         -- Reading a file from an S3 requires downloading
         -- and concatenating all the parts.
         let getS3 = \_ _ object ->
                     do allObjects <- readMVar files
                        let objects = filter (object `isPrefixOf`) allObjects
                        when (null objects) $ -- Object doesn't exist. Throw an error.
                                              -- Code in MACID.Checkpoint relies on the
                                              -- error to break a loop.
                             ioError $ mkIOError doesNotExistErrorType object Nothing Nothing
                        strs <- flip mapM objects $ \obj ->
                                  S3.sendRequest s3 (S3.getObject s3 bucket obj)
                        return (map P.pack strs)
             replaceS3 = \_ object info ->
                         S3.sendRequest_ s3 (S3.createObject s3 bucket object info)
         saverQueue chan
           bucket object
           (\_ object -> do allObjects <- readMVar files
                            let objects = filter (object `isPrefixOf`) allObjects
                            nVar <- newMVar (length objects)
                            return nVar)
           (\_ -> return ()) -- Closing the S3 backend is a no-op. We can't close the S3
                             -- connection because others might be using it.
           getS3
           replaceS3


