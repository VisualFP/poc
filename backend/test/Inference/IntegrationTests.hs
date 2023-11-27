module Inference.IntegrationTests where

import VFP.Inference.Example
import VFP.Inference.InputModel
import VFP.Inference.Elaboration
import VFP.Inference.Unification
import VFP.Inference.Zonking
import Test.HUnit

-- =================== utilities

runPipeline :: InputExpression -> InferenceResult
runPipeline ex =
    let (elaboratedExpression, typeConstraints) = elaboration ex
        unifcationResult = unification typeConstraints
        zonked = zonking elaboratedExpression unifcationResult
    in zonked

assertInferenceError :: InferenceResult -> IO ()
assertInferenceError (Right v) = assertFailure $ "Expected failure, got result: " ++ show v
assertInferenceError (Left _) = return ()

assertType :: InferedType -> Maybe InferedType -> IO ()
assertType expected = assertEqual "Assert types Equal" (Just expected)

getTopmostType :: InferenceResult -> Maybe InferedType
getTopmostType (Right (InferedConstant _ typ)) = Just typ
getTopmostType (Right (InferedTypeHole _ typ)) = Just typ
getTopmostType (Right (InferedApplication _ _ typ)) = Just typ
getTopmostType (Right (InferedTuple _ _ typ)) = Just typ
getTopmostType (Right (InferedLambda _ _ typ)) = Just typ
getTopmostType (Left _) = Nothing

findTypeOfIdentifier :: String -> InferenceResult -> Maybe InferedType
findTypeOfIdentifier _ (Left _) = Nothing
findTypeOfIdentifier expected (Right result) =
    case filter (\x -> fst x == expected) $ enumerateConstants result of [] -> error "type-hole not found" ; (_,x):_ -> Just x
    where
        enumerateConstants :: InferedExpression -> [(String, InferedType)]
        enumerateConstants input = case input of
            InferedConstant n t -> [(n, t)]
            InferedTypeHole n t -> [(n, t)]
            InferedApplication l r _ -> enumerateConstants l ++ enumerateConstants r
            InferedTuple l r _ -> enumerateConstants l ++ enumerateConstants r
            InferedLambda (variableName, variableType) nested _ -> (variableName,variableType) : enumerateConstants nested

-- =================== Tests

simpleAdditionTest :: Test
simpleAdditionTest = TestLabel "simpleAdditionTest" $ TestCase $
    let input = InputApplication InputUnknownType
            (InputConstant (InputFunction inputInt inputInt) "add2")
            (InputConstant inputInt "2")
        expected = inferedInt
        result = runPipeline input
    in assertType expected $ getTopmostType result

nestedApplicationTest :: Test
nestedApplicationTest = TestLabel "nestedApplication" $ TestCase $
    let input = InputApplication InputUnknownType
            (InputApplication InputUnknownType
                (InputConstant (InputFunction inputInt (InputFunction inputInt inputInt)) "add2")
                (InputConstant inputInt "2"))
            (InputConstant inputInt  "2")
        expected = inferedInt
        result = runPipeline input
    in assertType expected $ getTopmostType result

typeHoleTest :: Test
typeHoleTest = TestLabel "typeHoleTest" $ TestCase $
    let input = InputApplication inputInt
            (InputApplication inputUnknownType
                (InputConstant (InputFunction inputInt (InputFunction inputInt inputInt)) "add2")
                (InputConstant inputUnknownType  "?1"))
            (InputConstant inputInt  "2")
        expected = inferedInt
        result = runPipeline input
    in assertType expected $ findTypeOfIdentifier "?1" result

typeHoleFunctionInTypedApplicationTest :: Test
typeHoleFunctionInTypedApplicationTest = TestLabel "typeHoleFunctionInTypedApplicationTest" $ TestCase $
    let input =  InputApplication inputInt
            (InputApplication inputUnknownType
                (InputConstant inputUnknownType  "?1")
                (InputConstant inputInt "3"))
            (InputConstant inputInt  "2")
        expected = InferedFunctionType inferedInt (InferedFunctionType inferedInt inferedInt)
        result = runPipeline input
    in assertType expected $ findTypeOfIdentifier "?1" result

variableReuseTest :: Test
variableReuseTest = TestLabel "variableReuseTest" $ TestCase $
    let input = InputTuple inputUnknownType
            (InputConstant inputInt  "a")
            (InputConstant InputUnknownType  "a")
        expected = InferedTupleType inferedInt inferedInt
        result = runPipeline input
    in assertType expected $ getTopmostType result

simpleLambdaTest :: Test
simpleLambdaTest = TestLabel "simpleLambdaTest" $ TestCase $
    let input = InputLambda (InputFunction inputInt (InputTupleType inputInt inputInt)) "a"
            (InputTuple inputUnknownType (InputConstant InputUnknownType "a") (InputConstant InputUnknownType "a"))
        expected = InferedFunctionType inferedInt $ InferedTupleType inferedInt inferedInt
        result = runPipeline input
    in assertType expected $ getTopmostType result

lambdaArgumentInferenceTest :: Test
lambdaArgumentInferenceTest = TestLabel "lambdaArgumentInferenceTest" $ TestCase $
    let input = InputLambda InputUnknownType "a" (InputApplication InputUnknownType (InputApplication InputUnknownType inputPlus (InputConstant InputUnknownType "a")) (InputConstant InputUnknownType "a"))
        expected = InferedFunctionType inferedInt inferedInt
        result = runPipeline input
    in assertType expected $ getTopmostType result

lambdaWrongReturnTypeTest :: Test
lambdaWrongReturnTypeTest = TestLabel "lambdaWrongReturnTypeTest" $ TestCase $
    let input = InputLambda (InputFunction inputInt inputInt) "a"
            (InputConstant inputString "other")
        result = runPipeline input
    in assertInferenceError result

lambdaCrossReferenceTest :: Test
lambdaCrossReferenceTest = TestLabel "lambdaCrossReferenceTest" $ TestCase $
    let input =  InputApplication inputInt
            (InputApplication inputUnknownType
                (InputConstant (InputFunction (InputFunction inputUnknownType inputUnknownType) inputUnknownType) "a")
                (InputLambda (InputFunction inputInt inputInt) "v" $ InputConstant inputInt "v"))
            (InputConstant inputUnknownType "v")
        result = runPipeline input
    in assertInferenceError result

lambdaTypeInferenceTest :: Test
lambdaTypeInferenceTest = TestLabel "lambdaTypeInferenceTest" $ TestCase $
    let input = InputValueDefinition (InputFunction inputInt inputInt) (InputLambda inputUnknownType "a" $ InputTypeHole inputUnknownType)
        result = runPipeline input
    in case result of
        Right (InferedLambda (_, paramType) lambdaBody lambdaType) -> do
            assertType inferedInt (Just paramType)
            assertType (InferedFunctionType inferedInt inferedInt) (Just lambdaType)
            case lambdaBody of
                InferedTypeHole _ holeType -> assertType inferedInt (Just holeType)
                _ -> assertFailure "Found unexpected expression type"
        Right _ -> assertFailure "Found unexpected expression type"
        Left _ -> assertFailure "Failed to infere lambda type"

integrationTests :: Test
integrationTests = TestLabel "IntegrationTests" $ TestList [
    simpleAdditionTest,
    nestedApplicationTest,
    typeHoleTest,
    typeHoleFunctionInTypedApplicationTest,
    variableReuseTest,
    simpleLambdaTest,
    lambdaArgumentInferenceTest,
    lambdaWrongReturnTypeTest,
    lambdaCrossReferenceTest,
    lambdaTypeInferenceTest
    ]