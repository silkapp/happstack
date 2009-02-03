module HAppS.State.Tests.Helpers where

import HAppS.State.Saver
import HAppS.State.Saver.Impl.Memory

import System.Directory
import System.Random
import System.FilePath
import System.Exit
import System.IO.Unsafe
import Control.Monad
import Control.Exception
import Control.Concurrent

import Test.QuickCheck
import Test.QuickCheck.Batch
import Text.Printf

instance Arbitrary Char where
    arbitrary = choose ('a','z')
    coarbitrary = undefined

instance (Arbitrary a, Num a) => Arbitrary (Abs a) where
    arbitrary = liftM (Abs . abs) arbitrary
    coarbitrary = undefined

instance (Arbitrary a) => Arbitrary (NonEmpty a) where
    arbitrary = do x <- arbitrary
                   xs <- arbitrary
                   return $ NonEmpty (x:xs)
    coarbitrary = undefined

data Abs a = Abs a
instance (Show a) => Show (Abs a) where
    show (Abs a) = show a

data NonEmpty a = NonEmpty [a]
instance (Show a) => Show (NonEmpty a) where
    show (NonEmpty a) = show a

ioProperty = unsafePerformIO

withTemporaryDirectory :: (FilePath -> IO a) -> IO a
withTemporaryDirectory action
    = do tmp <- getTemporaryDirectory
         n <- randomIO
         let dir = tmp </> (show (abs n :: Int))
         exist <- doesDirectoryExist dir
         if exist
            then withTemporaryDirectory action
            else bracket_ (createDirectoryIfMissing False dir)
                          (removeDirectoryRecursive dir)
                          (action dir)

withMemorySaver action = action =<< liftM Memory newMemoryStore

withFileSaver action = withTemporaryDirectory $ \dir -> action (FileSaver dir)

withQueueSaver h action = h (action . Queue)

forEachSaver action
    = forM savers $ \(name, withSaver) ->
      action name withSaver
  where savers = [ ("Memory", withMemorySaver)
                 , ("Queue Memory", \action -> withMemorySaver (action . Queue)) 
                 , ("File", withFileSaver)
                 , ("Queue File", \action -> withFileSaver (action . Queue))
                 ]


forEachSaver_ action
    = forM_ savers $ \(name, withSaver) ->
      action name withSaver
  where savers = [ ("Memory", withMemorySaver)
                 , ("Queue Memory", \action -> withMemorySaver (action . Queue)) 
                 , ("File", withFileSaver)
                 , ("Queue File", \action -> withFileSaver (action . Queue))
                 ]

tryTests :: String -> TestOptions -> [TestOptions -> IO TestResult] -> IO ()
tryTests name opt tests
    = do printf "%25s : " name
         forM_ tests $ \test ->
             do res <- test opt
                case res of
                  TestOk _ _ _ -> putStr "."
                  TestExausted _ _ _ -> putStr "?"
                  TestFailed err num -> do putStr "\n"
                                           printf "   ** test %d of %s failed with the binding(s)\n" num name
                                           sequence_ [ putStrLn ("   **   " ++ v) | v <- err ]
                                           exitWith (ExitFailure 1)
                  TestAborted exp -> do putStr "\n"
                                        printf "   ** test %s failed with the exception\n" name
                                        putStrLn ("   **   " ++ show exp)
                                        exitWith (ExitFailure 1)
         putChar '\n'
