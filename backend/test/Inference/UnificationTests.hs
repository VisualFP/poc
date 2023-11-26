module Inference.UnificationTests where

import VFP.Inference.Unification
import VFP.Inference.Example
import Test.HUnit
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

listOfBoolTest :: Test
listOfBoolTest = TestLabel "listOfBoolTest" $ TestCase $
    let input = Set.fromList [ (unifAlpha, unifList unifBeta), (unifAlpha, unifList unifBool) ]
        expectedResolved = Map.fromList [ (unifBeta, unifBool), (unifAlpha, unifList unifBool)]
        expectedResiduals = Set.fromList []
        (resultResiduals, resultResolved) = unification input
    in do
        assertEqual "resolveds" expectedResolved resultResolved
        assertEqual "residuals" expectedResiduals resultResiduals

contradictaryTest :: Test
contradictaryTest = TestLabel "contradicatryTest" $ TestCase $
    let input = Set.fromList [ (unifAlpha, unifList unifString), (unifAlpha, unifList unifBool) ]
        expectedResolved = Map.fromList [ (unifAlpha, unifList unifBool) ]
        (resultResiduals, resultResolved) = unification input
    in do
        assertEqual "resolveds" expectedResolved resultResolved
        assertBool "residuals not null" (not $ Set.null resultResiduals)

multipleIndirectionsTest :: Test
multipleIndirectionsTest = TestLabel "multipleIndirectionsTest" $ TestCase $
    let input = Set.fromList [ (unifAlpha, unifBeta), (unifDelta, unifList unifBool), (unifAlpha, unifDelta), (unifDelta, unifBeta), (unifDelta, unifList unifGamma) ]
        expectedResolved = Map.fromList [ (unifGamma, unifBool), (unifBeta, unifList unifBool), (unifDelta, unifList unifBool), (unifAlpha, unifList unifBool) ]
        expectedResiduals = Set.fromList []
        (resultResiduals, resultResolved) = unification input
    in do
        assertEqual "resolveds" expectedResolved resultResolved
        assertEqual "residuals" expectedResiduals resultResiduals

simpleFunctionTest :: Test
simpleFunctionTest = TestLabel "simpleFunctionTest" $ TestCase $
    let input = Set.fromList [ (unifAlpha, unifFunction unifInt unifBeta), (unifBeta, unifBool) ]
        expectedResolved = Map.fromList [ (unifBeta, unifBool), (unifAlpha, unifFunction unifInt unifBool) ]
        expectedResiduals = Set.fromList []
        (resultResiduals, resultResolved) = unification input
    in do
        assertEqual "resolveds" expectedResolved resultResolved
        assertEqual "residuals" expectedResiduals resultResiduals

unsolvableTest :: Test
unsolvableTest = TestLabel "unsolvableTest" $ TestCase $
    let input = Set.fromList [ (unifAlpha, unifBeta), (unifBeta, unifAlpha) ]
        expectedResolved = Map.empty
        (resultResiduals, resultResolved) = unification input
    in do
        assertEqual "resolveds" expectedResolved resultResolved
        assertBool "residuals not null" (not $ Set.null resultResiduals)

unificationTests :: Test
unificationTests = TestLabel "UnificationTests" $ TestList [listOfBoolTest, contradictaryTest, multipleIndirectionsTest, simpleFunctionTest, unsolvableTest]
