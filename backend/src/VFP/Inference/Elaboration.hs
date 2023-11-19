{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module VFP.Inference.Elaboration(ElaboratedExpression(..), elaboration) where

import VFP.Inference.InputModel
import VFP.Inference.Unification
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Control.Monad.State.Lazy

data ElaboratedExpression = ElaboratedConstant UnificationType String 
                          | ElaboratedTuple UnificationType ElaboratedExpression ElaboratedExpression 
                          | ElaboratedApplication UnificationType ElaboratedExpression ElaboratedExpression 
                          | ElaboratedLambda UnificationType (String, UnificationType) ElaboratedExpression 
                          deriving Show

unificationFunctionType :: UnificationType -> UnificationType -> UnificationType
unificationFunctionType from to = UnificationConstructedType "->" [from, to]

unificationTupleType :: UnificationType -> UnificationType -> UnificationType
unificationTupleType l r = UnificationConstructedType "(,)" [l, r]

data ElaborationStateValue = ElaborationState {variableCounter :: Int, constraints :: TypeConstraintConjunction, variablesToReuse :: Map.Map String UnificationType} deriving Show
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
initialElaborationStateValue = ElaborationState{variableCounter = 0, constraints = Set.empty, variablesToReuse = Map.empty}

setupVariableReuse :: String -> UnificationType -> ElaborationState ()
setupVariableReuse name typ = do
    existing <- getVariableToReuse name
    case existing of
        Nothing -> addVariableToReuse name typ
        Just e -> addElaboratedConstraint (typ, e) 

getNextVariable :: ElaborationState UnificationType
getNextVariable = do
    s <- get
    let currentCounter = variableCounter s
    put $ s{variableCounter = currentCounter + 1}
    return $ UnificationVariable $ "UV" ++ show currentCounter

addElaboratedConstraint :: TypeConstraint -> ElaborationState ()
addElaboratedConstraint c = do s <- get ; put s{constraints = Set.insert c $ constraints s}

inputToUnificationType :: InputType -> ElaborationState UnificationType
inputToUnificationType (InputPrimitive name) = return $ UnificationConstantType name
inputToUnificationType InputUnknownType = getNextVariable
inputToUnificationType (InputTupleType inputL inputR) = do
    elaboratedL <- inputToUnificationType inputL
    elaboratedR <- inputToUnificationType inputR
    return $ unificationTupleType elaboratedL elaboratedR
inputToUnificationType (InputFunction inputArg inputRet) = do
    elaboratedArg <- inputToUnificationType inputArg
    elaboratedRet <- inputToUnificationType inputRet
    return $ unificationFunctionType elaboratedArg elaboratedRet

elaborate :: InputExpression -> UnificationType -> ElaborationState ElaboratedExpression
elaborate input toFill = do
    unificationType <- inputToUnificationType $ getInputType input
    addElaboratedConstraint (toFill, unificationType)
    case input of
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
