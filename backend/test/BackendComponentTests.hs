module BackendComponentTests where

import VFP.Translation.Inference
import Test.HUnit
import VFP.UI.UIModel

-- These tests have been build in combination with VFP.Translation.TranslateToHaskellSource,
-- and thus use complex but valueable test data

lambdaAtTopMostTypeHole :: Test
lambdaAtTopMostTypeHole = TestLabel "lambdaAtTopMostTypeHole" $ TestCase $
    let input =  ValueDefinition (Just (Generic 1)) "userDefinedFunction" (Lambda (Just (Generic 1)) TypeHole)
        expected = TypedValueDefinition (Function (Generic 1) (Generic 2)) "userDefinedFunction" (TypedLambda (Function (Generic 1) (Generic 2)) (Generic 1,"i") (TypedTypeHole (Generic 2) "?0"))
        result = infere input
    in case result of
        Success r -> assertEqual "typed value equals" expected r
        Error _ -> assertFailure "inference failed"

recursiveCall :: Test
recursiveCall = TestLabel "recursiveCall" $ TestCase $
    let input = ValueDefinition (Just (Generic 1)) "userDefinedFunction" (Reference (Just (Generic 1)) "userDefinedFunction" (ToFill (Generic 1)))
        expected = TypedValueDefinition (Generic 1) "userDefinedFunction" (TypedReference (Generic 1) "userDefinedFunction" [])
        result = infere input
    in case result of
        Success r -> assertEqual "typed value equals" expected r
        Error _ -> assertFailure "inference failed"

lambdaParameterScopeViolation :: Test
lambdaParameterScopeViolation = TestLabel "lambdaParameterScopeViolation" $ TestCase $
    let input = ValueDefinition (Just (List (Generic 1))) "userDefinedFunction" (Reference (Just (Function (Function (Generic 2) (Generic 1)) (Function (List (Generic 2)) (List (Generic 1))))) "map" (ArgumentList [Lambda (Just (Function (Generic 2) (Generic 1))) TypeHole,Reference (Just (Generic 2)) "i" (ToFill (List (Generic 2)))]))
        expected = "Scope violation: \"i\" cannot be used here"
        result = infere input
    in case result of
        Success _ -> assertFailure "expected inference to fail"
        Error m -> assertEqual "assert error message" m expected

boolIntoStringTypeHole :: Test
boolIntoStringTypeHole = TestLabel "boolIntoStringTypeHole" $ TestCase $
    let input = ValueDefinition (Just (Primitive "String")) "userDefinedFunction" (Reference (Just (Function (Primitive "Int") (Primitive "String"))) "intToString" (ArgumentList [BooleanLiteral "True"]))
        expected = "Inference failed: Could not unify \"Bool ~ Int\""
        result = infere input
    in case result of
        Success _ -> assertFailure "expected inference to fail"
        Error m -> assertEqual "assert error message" m expected

lambdaParameterAsLastArgOfFold :: Test
lambdaParameterAsLastArgOfFold = TestLabel "lambdaParameterAsLastArgOfFold" $ TestCase $
    let input = ValueDefinition (Just (Function (Generic 1) (Generic 2))) "userDefinedFunction" (Lambda (Just (Function (Generic 1) (Generic 2))) (Reference (Just (Function (Function (Generic 3) (Function (Generic 2) (Generic 2))) (Function (Generic 2) (Function (List (Generic 3)) (Generic 2))))) "fold" (ArgumentList [TypeHole,TypeHole,Reference (Just (Generic 1)) "i" (ToFill (List (Generic 3)))])))
        expected = TypedValueDefinition (Function (List (Generic 1)) (Generic 2)) "userDefinedFunction" (TypedLambda (Function (List (Generic 1)) (Generic 2)) (List (Generic 1),"i") (TypedReference (Function (Function (Generic 1) (Function (Generic 2) (Generic 2))) (Function (Generic 2) (Function (List (Generic 1)) (Generic 2)))) "fold" [TypedTypeHole (Function (Generic 1) (Function (Generic 2) (Generic 2))) "?0",TypedTypeHole (Generic 2) "?1",TypedReference (List (Generic 1)) "i" []]))
        result = infere input
    in case result of
        Success r -> assertEqual "typed value equals" expected r
        Error _ -> assertFailure "inference failed"

nestedLambdas :: Test
nestedLambdas = TestLabel "nestedLambdas" $ TestCase $
    let input = ValueDefinition (Just (Function (Primitive  "Int") (Function (Primitive  "Int") (Primitive  "Int")))) "userDefinedFunction" (Lambda (Just (Function (Primitive  "Int") (Function (Primitive  "Int") (Primitive  "Int")))) (Lambda (Just (Function (Primitive  "Int") (Primitive  "Int"))) (Reference (Just (Function (Primitive  "Bool") (Function (Primitive  "Int") (Function (Primitive  "Int") (Primitive  "Int"))))) "if" (ArgumentList [Reference (Just (Function (Primitive  "Int") (Function (Primitive  "Int") (Primitive  "Bool")))) "largerThan" (ArgumentList [Reference (Just (Primitive  "Int")) "i" (ArgumentList []),Reference (Just (Primitive  "Int")) "j" (ArgumentList [])]),Reference (Just (Primitive  "Int")) "i" (ArgumentList []),Reference (Just (Primitive  "Int")) "j" (ToFill (Primitive  "Int"))]))))
        expected = TypedValueDefinition (Function (Primitive  "Int") (Function (Primitive  "Int") (Primitive  "Int"))) "userDefinedFunction" (TypedLambda (Function (Primitive  "Int") (Function (Primitive  "Int") (Primitive  "Int"))) (Primitive  "Int","i") (TypedLambda (Function (Primitive  "Int") (Primitive  "Int")) (Primitive  "Int","j") (TypedReference (Function (Primitive  "Bool") (Function (Primitive  "Int") (Function (Primitive  "Int") (Primitive  "Int")))) "if" [TypedReference (Function (Primitive  "Int") (Function (Primitive  "Int") (Primitive  "Bool"))) "largerThan" [TypedReference (Primitive  "Int") "i" [],TypedReference (Primitive  "Int") "j" []],TypedReference (Primitive  "Int") "i" [],TypedReference (Primitive  "Int") "j" []])))
        result = infere input
    in case result of
        Success r -> assertEqual "typed value equals" expected r
        Error _ -> assertFailure "inference failed"

threeNestedIdentities :: Test
threeNestedIdentities = TestLabel "threeNestedIdentities" $ TestCase $
    let input = ValueDefinition (Just (Generic 1)) "userDefinedFunction" (Reference (Just (Function (Generic 1)(Generic 1))) "identity" (ArgumentList [Reference (Just (Function (Generic 1)(Generic 1))) "identity" (ArgumentList [Reference (Just (Function (Generic 1)(Generic 1))) "identity" (ToFill (Generic 1))])]))
        expected = TypedValueDefinition (Generic 1) "userDefinedFunction" (TypedReference (Function (Generic 1)(Generic 1)) "identity" [(TypedReference (Function (Generic 1)(Generic 1)) "identity" [(TypedReference (Function (Generic 1)(Generic 1)) "identity" [(TypedTypeHole ((Generic 1)) "?0")])])])
        result = infere input
    in case result of
        Success r -> assertEqual "typed value equals" expected r
        Error _ -> assertFailure "inference failed"

backendComponentTests :: Test
backendComponentTests = TestLabel "ComponentTests" $ TestList [
    lambdaAtTopMostTypeHole,
    lambdaParameterScopeViolation,
    recursiveCall,
    boolIntoStringTypeHole,
    lambdaParameterAsLastArgOfFold,
    nestedLambdas,
    threeNestedIdentities
    ]