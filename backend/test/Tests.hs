-- Copyright (C) 2023 Lukas Streckeisen & Jann Flepp

import Test.HUnit
import Inference.UnificationTests
import Inference.IntegrationTests
import System.Exit
import UI.UIModelTests (uiModelTests)

main :: IO ()
main = do
  results <- runTestTT $ TestLabel "VFPInference-Tests" $ TestList [ unificationTests, integrationTests, uiModelTests ]
  if errors results + failures results == 0
    then
      exitSuccess
    else
      exitWith (ExitFailure 1)