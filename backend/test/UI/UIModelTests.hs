-- Copyright (C) 2023 Lukas Streckeisen & Jann Flepp
module UI.UIModelTests where

import Test.HUnit
import VFP.UI.UIModel
import Data.List (isInfixOf)

insertLiteralIntoFirstReferenceArgument :: Test
insertLiteralIntoFirstReferenceArgument =
  TestLabel "insertLiteralIntoFirstReferenceArgument" $
    TestCase $
      let literal = StringLiteral $ Just "testLiteral"
          original = TypedReference (Function (Primitive "string") (Function (Primitive "string") (Primitive "string"))) "testFunction" [TypedTypeHole (Primitive "string") "testTypeHole1", TypedTypeHole (Primitive "string") "testTypeHole2"]
          expected = Reference (Just (Function (Primitive "string") (Function (Primitive "string") (Primitive "string")))) "testFunction" (ArgumentList [StringLiteral $ Just "testLiteral", TypeHole])
          result = insertUntypedValueIntoTypeHole literal "testTypeHole1" original
       in case result of
            Left e -> assertFailure $ "expected insert to be successful but got error: " ++ e
            Right r -> assertEqual "result" expected r

insertLiteralIntoSecondReferenceArgument :: Test
insertLiteralIntoSecondReferenceArgument =
  TestLabel "insertLiteralIntoSecondReferenceArgument" $
    TestCase $
      let literal = StringLiteral $ Just "testLiteral"
          original = TypedReference (Function (Primitive "string") (Function (Primitive "string") (Primitive "string"))) "testFunction" [TypedTypeHole (Primitive "string") "testTypeHole1", TypedTypeHole (Primitive "string") "testTypeHole2"]
          expected = Reference (Just (Function (Primitive "string") (Function (Primitive "string") (Primitive "string")))) "testFunction" (ArgumentList [TypeHole, StringLiteral $ Just "testLiteral"])
          result = insertUntypedValueIntoTypeHole literal "testTypeHole2" original
       in case result of
            Left e -> assertFailure $ "expected insert to be successful but got error: " ++ e
            Right r -> assertEqual "result" expected r

insertIntLiteralAndCompleteReferenceArguments :: Test
insertIntLiteralAndCompleteReferenceArguments =
  TestLabel "insertIntLiteralAndCompleteReferenceArguments" $
    TestCase $
      let literal = IntegerLiteral $ Just "2"
          original = TypedReference (Function (Primitive "int") (Function (Primitive "int") (Primitive "int"))) "testFunction" [TypedLiteral (Primitive "int") "4", TypedTypeHole (Primitive "int") "testTypeHole1"]
          expected = Reference (Just (Function (Primitive "int") (Function (Primitive "int") (Primitive "int")))) "testFunction" (ArgumentList [IntegerLiteral $ Just "4", IntegerLiteral $ Just "2"])
          result = insertUntypedValueIntoTypeHole literal "testTypeHole1" original
       in case result of
            Left e -> assertFailure $ "expected insert to be successful but got error: " ++ e
            Right r -> assertEqual "result" expected r

conversionFromUnknownLiteralToUntypedValueFails :: Test
conversionFromUnknownLiteralToUntypedValueFails = TestLabel "conversionFromUnknownLiteralToUntypedValueFails" $ TestCase $
    let literal = IntegerLiteral $ Just "2"
        original = TypedReference (Function (Primitive "int") (Function (Primitive "int") (Primitive "int"))) "testFunction" [TypedLiteral (Primitive "char") "a", TypedTypeHole (Primitive "int") "testTypeHole1"]
        result = insertUntypedValueIntoTypeHole literal "testTypeHole1" original
    in case result of
        Left e -> assertBool "Insert fails due to unknown literal type" $ "Unknown" `isInfixOf` e
        Right r -> assertFailure $ "expected insert to fail but got: " ++ show r

insertReferenceIntoLambdaBody :: Test
insertReferenceIntoLambdaBody =
  TestLabel "insertReferenceIntoLambdaBody" $
    TestCase $
      let reference = Reference (Just $ Function (Primitive "string") (Primitive "string")) "testFunction" UnknownArgs
          original = TypedLambda (Function (Primitive "string") (Function (Primitive "string") (Primitive "string"))) (Primitive "string", "lambdaParam") (TypedTypeHole (Function (Primitive "string") (Primitive "string")) "typeHole1")
          expected = Lambda (Just $ Function (Primitive "string") (Function (Primitive "string") (Primitive "string"))) (LambdaValue (Reference (Just $ Function (Primitive "string") (Primitive "string")) "testFunction" (ToFill $ Function (Primitive "string") (Primitive "string"))))
          result = insertUntypedValueIntoTypeHole reference "typeHole1" original
       in case result of
            Left e -> assertFailure $ "expected insert to be successful but got error: " ++ e
            Right r -> assertEqual "result" expected r

insertLambdaIntoTypeHole :: Test
insertLambdaIntoTypeHole =
  TestLabel "insertLambdaIntoTypeHole" $
    TestCase $
      let lambda = Lambda (Just $ Function (Primitive "string") (Function (Primitive "string") (Primitive "string"))) ValueToFill
          original = TypedTypeHole (Function (Primitive "string") (Function (Primitive "string") (Primitive "string"))) "typeHole1"
          expected = Lambda (Just $ Function (Primitive "string") (Function (Primitive "string") (Primitive "string"))) ValueToFill
          result = insertUntypedValueIntoTypeHole lambda "typeHole1" original
       in case result of
            Left e -> assertFailure $ "expected insert to be successful but got error: " ++ e
            Right r -> assertEqual "result" expected r

uiModelTests :: Test
uiModelTests =
  TestLabel "UI Model Tests" $
    TestList
      [ insertLiteralIntoFirstReferenceArgument,
        insertLiteralIntoSecondReferenceArgument,
        insertIntLiteralAndCompleteReferenceArguments,
        conversionFromUnknownLiteralToUntypedValueFails,
        insertReferenceIntoLambdaBody,
        insertLambdaIntoTypeHole
      ]