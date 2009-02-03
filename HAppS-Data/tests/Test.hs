module Main where

import Test.HUnit (errors, failures, putTextToShowS,runTestText)
import System.Exit (exitFailure)
import HAppS.Data.Tests (allTests)

-- |A simple driver for running the local test suite.
main :: IO ()
main =
    do (c,st) <- runTestText putTextToShowS allTests
       putStrLn (st "")
       case (failures c) + (errors c) of
         0 -> return ()
         n -> exitFailure