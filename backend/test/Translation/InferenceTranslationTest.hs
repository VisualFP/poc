module Translation.InferenceTranslationTest where

import Test.HUnit
import VFP.Translation.InferenceTranslation
import VFP.UI.UIModel as UI

stringDoesNotMatchIntBaseValueTypeHole :: Test
stringDoesNotMatchIntBaseValueTypeHole = TestLabel "stringDoesNotMatchIntBaseValueTypeHole" $ TestCase $
    let valueUnderConstruction = UI.UntypedValueUnderConstruction (UI.Primitive "int") (UI.StringLiteral $ Just "Hi")
        inferenceResult = infere valueUnderConstruction
    in case inferenceResult of
        Error e -> assertBool "string literal doesn't match type hole of int base type hole" $ not $ null e
        Success _ -> assertFailure "string literal shouldn't match type hole of int base type hole"

intMatchesIntBaseValueTypeHole :: Test
intMatchesIntBaseValueTypeHole = TestLabel "stringDoesNotMatchIntBaseValueTypeHole" $ TestCase $
    let valueUnderConstruction = UI.UntypedValueUnderConstruction (UI.Primitive "int") (UI.IntegerLiteral $ Just "5")
        expected = TypedLiteral (Primitive "int") "5"
        inferenceResult = infere valueUnderConstruction
    in case inferenceResult of
        Error e -> assertFailure $ "int literal doesn't match type hole of int base type hole: " ++ e
        Success r -> assertEqual "int literal matches type hole of int base type hole" r expected

inferenceTranslationTests :: Test
inferenceTranslationTests = TestList
    [
        stringDoesNotMatchIntBaseValueTypeHole,
        intMatchesIntBaseValueTypeHole
    ]
