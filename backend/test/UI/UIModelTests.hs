-- Copyright (C) 2023 Lukas Streckeisen & Jann Flepp
module UI.UIModelTests where

import Test.HUnit
import VFP.UI.UIModel

insertLiteralIntoFirstReferenceArgument :: Test
insertLiteralIntoFirstReferenceArgument =
  TestLabel "insertLiteralIntoFirstReferenceArgument" $
    TestCase $
      let literal = StringLiteral $ Just "testLiteral"
          original = TypedReference (Function (Primitive "string") (Function (Primitive "string") (Primitive "string"))) "testFunction" [TypedTypeHole (Primitive "string") "testTypeHole1", TypedTypeHole (Primitive "string") "testTypeHole2"]
          expected = Reference (Just (Function (Primitive "string") (Function (Primitive "string") (Primitive "string")))) "testFunction" (ArgumentList [StringLiteral $ Just "testLiteral", TypeHole])
          result = insertUntypedValueIntoTypeHole literal "testTypeHole1" original
       in assertEqual "result" expected result

insertLiteralIntoSecondReferenceArgument :: Test
insertLiteralIntoSecondReferenceArgument =
  TestLabel "insertLiteralIntoSecondReferenceArgument" $
    TestCase $
      let literal = StringLiteral $ Just "testLiteral"
          original = TypedReference (Function (Primitive "string") (Function (Primitive "string") (Primitive "string"))) "testFunction" [TypedTypeHole (Primitive "string") "testTypeHole1", TypedTypeHole (Primitive "string") "testTypeHole2"]
          expected = Reference (Just (Function (Primitive "string") (Function (Primitive "string") (Primitive "string")))) "testFunction" (ArgumentList [TypeHole, StringLiteral $ Just "testLiteral"])
          result = insertUntypedValueIntoTypeHole literal "testTypeHole2" original
       in assertEqual "result" expected result

insertIntLiteralAndCompleteReferenceArguments :: Test
insertIntLiteralAndCompleteReferenceArguments =
  TestLabel "insertIntLiteralAndCompleteReferenceArguments" $
    TestCase $
      let literal = IntegerLiteral $ Just "2"
          original = TypedReference (Function (Primitive "Int") (Function (Primitive "Int") (Primitive "Int"))) "testFunction" [TypedLiteral (Primitive "Int") "4", TypedTypeHole (Primitive "Int") "testTypeHole1"]
          expected = Reference (Just (Function (Primitive "Int") (Function (Primitive "Int") (Primitive "Int")))) "testFunction" (ArgumentList [IntegerLiteral $ Just "4", IntegerLiteral $ Just "2"])
          result = insertUntypedValueIntoTypeHole literal "testTypeHole1" original
       in assertEqual "result" expected result

insertReferenceIntoLambdaBody :: Test
insertReferenceIntoLambdaBody =
  TestLabel "insertReferenceIntoLambdaBody" $
    TestCase $
      let reference = Reference (Just $ Function (Primitive "string") (Primitive "string")) "testFunction" UnknownArgs
          original = TypedLambda (Function (Primitive "string") (Function (Primitive "string") (Primitive "string"))) (Primitive "string", "lambdaParam") (TypedTypeHole (Function (Primitive "string") (Primitive "string")) "typeHole1")
          expected = Lambda (Just $ Function (Primitive "string") (Function (Primitive "string") (Primitive "string"))) (Reference (Just $ Function (Primitive "string") (Primitive "string")) "testFunction" (ToFill $ Function (Primitive "string") (Primitive "string")))
          result = insertUntypedValueIntoTypeHole reference "typeHole1" original
       in assertEqual "result" expected result

insertLambdaIntoTypeHole :: Test
insertLambdaIntoTypeHole =
  TestLabel "insertLambdaIntoTypeHole" $
    TestCase $
      let lambda = Lambda (Just $ Function (Primitive "string") (Function (Primitive "string") (Primitive "string"))) TypeHole
          original = TypedTypeHole (Function (Primitive "string") (Function (Primitive "string") (Primitive "string"))) "typeHole1"
          expected = Lambda (Just $ Function (Primitive "string") (Function (Primitive "string") (Primitive "string"))) TypeHole
          result = insertUntypedValueIntoTypeHole lambda "typeHole1" original
       in assertEqual "result" expected result

uiModelTests :: Test
uiModelTests =
  TestLabel "UI Model Tests" $
    TestList
      [ insertLiteralIntoFirstReferenceArgument,
        insertLiteralIntoSecondReferenceArgument,
        insertIntLiteralAndCompleteReferenceArguments,
        insertReferenceIntoLambdaBody,
        insertLambdaIntoTypeHole
      ]