-- Copyright (C) 2023 Lukas Streckeisen & Jann Flepp

import Test.HUnit
import Inference.UnificationTests
import Inference.InferenceComponentTests
import IntegrationTests
import System.Exit
import UI.UIModelTests (uiModelTests)
import Translation.InferenceTranslationTest (inferenceTranslationTests)

main :: IO ()
main = do
  results <- runTestTT $ TestLabel "VFPInference-Tests" $ TestList [ integrationTests, unificationTests, inferenceComponentTests, uiModelTests, inferenceTranslationTests ]
  if errors results + failures results == 0
    then
      exitSuccess
    else
      exitWith (ExitFailure 1)