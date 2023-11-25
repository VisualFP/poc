{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module VFP.Inference.Elaboration(ElaboratedExpression(..), elaboration) where

import VFP.Inference.InputModel
import VFP.Inference.Unification
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Control.Monad.State.Lazy

import Debug.Trace

data ElaboratedExpression = ElaboratedConstant UnificationType String
                          | ElaboratedTuple UnificationType ElaboratedExpression ElaboratedExpression
                          | ElaboratedApplication UnificationType ElaboratedExpression ElaboratedExpression
                          | ElaboratedLambda UnificationType (String, UnificationType) ElaboratedExpression
                          | ElaboratedTypeHole UnificationType String
                          deriving Show

unificationFunctionType :: UnificationType -> UnificationType -> UnificationType
unificationFunctionType from to = UnificationConstructedType "->" [from, to]

unificationTupleType :: UnificationType -> UnificationType -> UnificationType
unificationTupleType l r = UnificationConstructedType "(,)" [l, r]

unificationListType :: UnificationType -> UnificationType
unificationListType i = UnificationConstructedType "[]" [i]

data ElaborationStateValue = ElaborationState {
    variableCounter :: Int,
    typeHoleCounter :: Int,
    constraints :: TypeConstraintConjunction,
    variablesToReuse :: Map.Map String UnificationType,
    generics :: Map.Map Int UnificationType} deriving Show
type ElaborationState = State ElaborationStateValue

addVariableToReuse :: String -> UnificationType -> ElaborationState ()
addVariableToReuse name typ = do
    s <- get
    put s{variablesToReuse = Map.insert name typ $ variablesToReuse s }

getVariableToReuse :: String -> ElaborationState (Maybe UnificationType)
getVariableToReuse name = do
    s <- get
    return $ Map.lookup name $ variablesToReuse s

addVariableToResuseForSubtree :: String -> UnificationType -> ElaborationState a -> ElaborationState a
addVariableToResuseForSubtree name typ action = do
    sBefore <- get
    put sBefore{variablesToReuse = Map.insert name typ $ variablesToReuse sBefore }
    r <- action
    sAfter <- get
    put sAfter{variablesToReuse = Map.delete name $ variablesToReuse sAfter }
    return r

initialElaborationStateValue :: ElaborationStateValue
initialElaborationStateValue = ElaborationState{variableCounter = 0, typeHoleCounter = 0, constraints = Set.empty, variablesToReuse = Map.empty, generics = Map.empty}

setupVariableReuse :: String -> UnificationType -> ElaborationState ()
setupVariableReuse name typ = do
    existing <- getVariableToReuse name
    case existing of
        Nothing -> addVariableToReuse name typ
        Just e -> addElaboratedConstraint (typ, e)

resetGenerics :: ElaborationState ()
resetGenerics = do
    s <- get
    put $ s{generics = Map.empty}

getOrCreateGeneric :: Int -> ElaborationState UnificationType
getOrCreateGeneric num = do
    s <- get
    let current = generics s
    case Map.lookup num current of
        Just typ -> return typ
        Nothing -> do
            typ <- getNextVariable
            _s <- get
            put $ _s{generics = Map.insert num typ current }
            return typ

getNextTypeHoleName :: ElaborationState String
getNextTypeHoleName = do
    s <- get
    let currentCounter = typeHoleCounter s
    put $ s{typeHoleCounter = currentCounter + 1}
    return $ "?" ++ show currentCounter

getNextVariable :: ElaborationState UnificationType
getNextVariable = do
    s <- get
    let currentCounter = variableCounter s
    put $ s{variableCounter = currentCounter + 1}
    return $ UnificationVariable $ "UV" ++ show currentCounter

addElaboratedConstraint :: TypeConstraint -> ElaborationState ()
addElaboratedConstraint c = do s <- get ; put s{constraints = Set.insert c $ constraints s}

inputToUnificationType :: InputType -> ElaborationState UnificationType
inputToUnificationType input = do 
        result <- _inputToUnificationType input
        resetGenerics
        return result
    where
        _inputToUnificationType :: InputType -> ElaborationState UnificationType
        _inputToUnificationType i = case i of
            InputPrimitive name -> return $ UnificationConstantType name
            InputGeneric num -> do
                getOrCreateGeneric num
            InputUnknownType -> getNextVariable
            InputTupleType inputL inputR -> do
                elaboratedL <- _inputToUnificationType inputL
                elaboratedR <- _inputToUnificationType inputR
                return $ unificationTupleType elaboratedL elaboratedR
            InputFunction inputArg inputRet -> do
                elaboratedArg <- _inputToUnificationType inputArg
                elaboratedRet <- _inputToUnificationType inputRet
                return $ unificationFunctionType elaboratedArg elaboratedRet
            InputList inner -> do
                elaboratedInner <- _inputToUnificationType inner
                return $ unificationListType elaboratedInner

elaborate :: InputExpression -> UnificationType -> ElaborationState ElaboratedExpression
elaborate input toFill = do
    unificationType <- inputToUnificationType $ getInputType input
    addElaboratedConstraint (toFill, unificationType)
    case input of
        InputValueDefinition definedType inner -> do
            unificationDefinedType <- inputToUnificationType definedType
            addElaboratedConstraint (toFill, unificationDefinedType)
            elaborate inner toFill
        InputTypeHole _ -> do
            typeHoleName <- getNextTypeHoleName
            let elaboratedExpression = ElaboratedTypeHole toFill typeHoleName
            return elaboratedExpression
        InputConstant _ name -> do
            setupVariableReuse name toFill
            let elaboratedExpression = ElaboratedConstant toFill name
            return elaboratedExpression
        InputTuple _ left right -> do
            leftType <- getNextVariable
            leftExpression <- elaborate left leftType
            rightType <- getNextVariable
            rightExpression <- elaborate right rightType
            let elaboratedExpression = ElaboratedTuple toFill leftExpression rightExpression
            addElaboratedConstraint (toFill, unificationTupleType leftType rightType)
            return elaboratedExpression
        InputApplication _ left right -> do
            leftType <- getNextVariable
            leftExpression <- elaborate left leftType
            rightType <- getNextVariable
            rightExpression <- elaborate right rightType
            let elaboratedExpression = ElaboratedApplication toFill leftExpression rightExpression
            addElaboratedConstraint (leftType, unificationFunctionType rightType toFill)
            return elaboratedExpression
        InputLambda _ variable nested -> do
            variableType <- getNextVariable
            nestedType <- getNextVariable
            nestedExpression <- addVariableToResuseForSubtree variable variableType $ elaborate nested nestedType
            let elaboratedExpression = ElaboratedLambda toFill (variable, variableType) nestedExpression
            addElaboratedConstraint (toFill, unificationFunctionType variableType nestedType)
            return elaboratedExpression

runElaboration :: InputExpression -> ElaborationState (ElaboratedExpression, TypeConstraintConjunction)
runElaboration input = do
    topVariable <- getNextVariable
    elaboratedExpression <- elaborate input topVariable
    finalState <- get
    return (elaboratedExpression, constraints finalState)

elaboration :: InputExpression -> (ElaboratedExpression, TypeConstraintConjunction)
elaboration _input = evalState (runElaboration _input) initialElaborationStateValue
