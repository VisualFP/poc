module Inference.IntegrationTests where

import VFP.Inference.Example
import VFP.Inference.InputModel
import VFP.Inference.Elaboration
import VFP.Inference.Unification
import VFP.Inference.Zonking
import Test.HUnit

-- =================== utilities

runPipeline :: InputExpression -> InferrenceResult
runPipeline ex =
    let (elaboratedExpression, typeConstraints) = elaboration ex
        unifcationResult = unification typeConstraints
        zonked = zonking elaboratedExpression unifcationResult
    in zonked

assertInferrenceError :: InferrenceResult -> IO ()
assertInferrenceError (Right v) = assertFailure $ "Expected failure, got result: " ++ show v
assertInferrenceError (Left _) = return ()

assertType :: InferredType -> Maybe InferredType -> IO ()
assertType expected = assertEqual "Assert types Equal" (Just expected)

getTopmostType :: InferrenceResult -> Maybe InferredType
getTopmostType (Right (InferredConstant _ typ)) = Just typ
getTopmostType (Right (InferredApplication _ _ typ)) = Just typ
getTopmostType (Right (InferredTuple _ _ typ)) = Just typ
getTopmostType (Right (InferredLambda _ _ typ)) = Just typ
getTopmostType (Left _) = Nothing

findTypeOfIdentifier :: String -> InferrenceResult -> Maybe InferredType
findTypeOfIdentifier _ (Left _) = Nothing
findTypeOfIdentifier expected (Right result) =
    case filter (\x -> fst x == expected) $ enumerateConstants result of [] -> error "type-hole not found" ; (_,x):_ -> Just x
    where
        enumerateConstants :: InferredExpression -> [(String, InferredType)]
        enumerateConstants input = case input of
            InferredConstant n t -> [(n, t)]
            InferredApplication l r _ -> enumerateConstants l ++ enumerateConstants r
            InferredTuple l r _ -> enumerateConstants l ++ enumerateConstants r
            InferredLambda (variableName, variableType) nested _ -> (variableName,variableType) : enumerateConstants nested

-- =================== Tests

simpleAdditionTest :: Test
simpleAdditionTest = TestLabel "simpleAdditionTest" $ TestCase $
    let input = InputApplication InputUnknownType
            (InputConstant (InputFunction inputInt inputInt) "add2")
            (InputConstant inputInt "2")
        expected = inferredInt
        result = runPipeline input
    in assertType expected $ getTopmostType result

nestedApplicationTest :: Test
nestedApplicationTest = TestLabel "nestedApplication" $ TestCase $
    let input = InputApplication InputUnknownType
            (InputApplication InputUnknownType
                (InputConstant (InputFunction inputInt (InputFunction inputInt inputInt)) "add2")
                (InputConstant inputInt "2"))
            (InputConstant inputInt  "2")
        expected = inferredInt
        result = runPipeline input
    in assertType expected $ getTopmostType result

typeHoleTest :: Test
typeHoleTest = TestLabel "typeHoleTest" $ TestCase $
    let input = InputApplication inputInt
            (InputApplication inputUnknownType
                (InputConstant (InputFunction inputInt (InputFunction inputInt inputInt)) "add2")
                (InputConstant inputUnknownType  "?1"))
            (InputConstant inputInt  "2")
        expected = inferredInt
        result = runPipeline input
    in assertType expected $ findTypeOfIdentifier "?1" result

typeHoleFunctionInTypedApplicationTest :: Test
typeHoleFunctionInTypedApplicationTest = TestLabel "typeHoleFunctionInTypedApplicationTest" $ TestCase $
    let input =  InputApplication inputInt
            (InputApplication inputUnknownType
                (InputConstant inputUnknownType  "?1")
                (InputConstant inputInt "3"))
            (InputConstant inputInt  "2")
        expected = inferredFunction inferredInt (inferredFunction inferredInt inferredInt)
        result = runPipeline input
    in assertType expected $ findTypeOfIdentifier "?1" result

variableReuseTest :: Test
variableReuseTest = TestLabel "variableReuseTest" $ TestCase $
    let input = InputTuple inputUnknownType
            (InputConstant inputInt  "a")
            (InputConstant InputUnknownType  "a")
        expected = inferredTuple inferredInt inferredInt
        result = runPipeline input
    in assertType expected $ getTopmostType result

simpleLambdaTest :: Test
simpleLambdaTest = TestLabel "simpleLambdaTest" $ TestCase $
    let input = InputLambda (InputFunction inputInt (InputTupleType inputInt inputInt)) "a"
            (InputTuple inputUnknownType (InputConstant InputUnknownType "a") (InputConstant InputUnknownType "a"))
        expected = inferredFunction inferredInt $ inferredTuple inferredInt inferredInt
        result = runPipeline input
    in assertType expected $ getTopmostType result

lambdaArgumentInferrenceTest :: Test
lambdaArgumentInferrenceTest = TestLabel "lambdaArgumentInferrenceTest" $ TestCase $
    let input = InputLambda InputUnknownType "a"
            (InputApplication InputUnknownType (InputApplication InputUnknownType inputPlus (InputConstant InputUnknownType "a")) (InputConstant InputUnknownType "a"))
        expected = inferredFunction inferredInt inferredInt
        result = runPipeline input
    in assertType expected $ getTopmostType result

lambdaWrongReturnTypeTest :: Test
lambdaWrongReturnTypeTest = TestLabel "lambdaWrongReturnTypeTest" $ TestCase $
    let input = InputLambda (InputFunction inputInt inputInt) "a"
            (InputConstant inputString "other")
        result = runPipeline input
    in assertInferrenceError result

lambdaCrossReferenceTest :: Test
lambdaCrossReferenceTest = TestLabel "lambdaCrossReferenceTest" $ TestCase $
    let input =  InputApplication inputInt
            (InputApplication inputUnknownType
                (InputConstant (InputFunction (InputFunction inputUnknownType inputUnknownType) inputUnknownType) "a")
                (InputLambda (InputFunction inputInt inputInt) "v" $ InputConstant inputInt "v"))
            (InputConstant inputUnknownType "v")
        result = runPipeline input
    in assertInferrenceError result

integrationTests :: Test
integrationTests = TestLabel "IntegrationTests" $ TestList [
    simpleAdditionTest,
    nestedApplicationTest,
    typeHoleTest,
    typeHoleFunctionInTypedApplicationTest,
    variableReuseTest,
    simpleLambdaTest,
    lambdaArgumentInferrenceTest,
    lambdaWrongReturnTypeTest ]