-- Copyright (C) 2023 Lukas Streckeisen & Jann Flepp

import Test.HUnit
import BackendComponentTests
import Inference.UnificationTests
import Inference.InferenceComponentTests
import System.Exit
import UI.UIModelTests
import Translation.InferenceTranslationTest

main :: IO ()
main = do
  results <- runTestTT $ TestLabel "VFPInference-Tests" $ TestList [ backendComponentTests, unificationTests, inferenceComponentTests, uiModelTests, inferenceTranslationTests ]
  if errors results + failures results == 0
    then
      exitSuccess
    else
      exitWith (ExitFailure 1)