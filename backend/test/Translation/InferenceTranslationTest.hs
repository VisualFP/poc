module Translation.InferenceTranslationTest where

import Test.HUnit
import VFP.Translation.Inference
import VFP.UI.UIModel as UI

stringDoesNotMatchIntBaseValueTypeHole :: Test
stringDoesNotMatchIntBaseValueTypeHole = TestLabel "stringDoesNotMatchIntBaseValueTypeHole" $ TestCase $
    let valueUnderConstruction = UI.ValueDefinition (Just $ UI.Primitive "Int") "test" (UI.StringLiteral $ Just "Hi")
        inferenceResult = infere valueUnderConstruction
    in case inferenceResult of
        Error e -> assertBool "string literal doesn't match type hole of int base type hole" $ not $ null e
        Success _ -> assertFailure "string literal shouldn't match type hole of Int base type hole"

infereInt :: Test
infereInt = TestLabel "infereInt" $ TestCase $
    let valueUnderConstruction = IntegerLiteral $ Just "5"
        expected = TypedLiteral (Primitive "Int") "5"
        inferenceResult = infere valueUnderConstruction
    in case inferenceResult of
        Error e -> assertFailure $ "Int literal doesn't match type hole of int base type hole: " ++ e
        Success r -> assertEqual "Int literal matches type hole of int base type hole" r expected

inferenceTranslationTests :: Test
inferenceTranslationTests = TestList
    [
        stringDoesNotMatchIntBaseValueTypeHole,
        infereInt
    ]
