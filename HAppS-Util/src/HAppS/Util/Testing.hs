module HAppS.Util.Testing (qctest, qccheck) where

import Test.HUnit as HU
import Test.QuickCheck as QC
import System.Random

qctest :: QC.Testable a => a -> Test
qctest = qccheck defaultConfig

qccheck :: QC.Testable a => Config -> a -> Test
qccheck config a = TestCase $
  do rnd <- newStdGen
     tests config (evaluate a) rnd 0 0 []

tests :: Config -> Gen Result -> StdGen -> Int -> Int -> [[String]] -> Assertion
tests config gen rnd0 ntest nfail stamps
  | ntest == configMaxTest config = return ()
  | nfail == configMaxFail config = assertFailure $ "Arguments exhausted after" ++ show ntest ++ (if ntest == 1 then " test." else " tests.")
  | otherwise               =
      do putStr (configEvery config ntest (arguments result))
         case ok result of
           Nothing    ->
             tests config gen rnd1 ntest (nfail+1) stamps
           Just True  ->
             tests config gen rnd1 (ntest+1) nfail (stamp result:stamps)
           Just False ->
             assertFailure $ ( "Falsifiable, after "
                   ++ show ntest
                   ++ " tests:\n"
                   ++ unlines (arguments result)
                    )
     where
      result      = generate (configSize config ntest) rnd2 gen
      (rnd1,rnd2) = split rnd0

