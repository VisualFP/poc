import Test.HUnit
import Inference.UnificationTests
import Inference.IntegrationTests
import System.Exit

main :: IO ()
main = do
  results <- runTestTT $ TestLabel "VFPInference-Tests" $ TestList [ unificationTests, integrationTests ]
  if errors results + failures results == 0
    then
      exitSuccess
    else
      exitWith (ExitFailure 1)