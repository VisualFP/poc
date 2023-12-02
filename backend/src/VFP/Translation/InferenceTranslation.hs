-- Copyright (C) 2023 Lukas Streckeisen & Jann Flepp

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module VFP.Translation.InferenceTranslation where

import qualified VFP.UI.UIModel as UI
import qualified VFP.Inference.InputModel as I
import qualified VFP.Inference.Zonking as O
import VFP.Inference.Elaboration (elaboration)
import VFP.Inference.Unification (unification)
import VFP.Inference.Zonking (zonking)
import Data.Char
import Control.Monad.State.Lazy
import Control.Monad (foldM)

import Debug.Trace

data InputTreeState = InputTreeState{genericCounter::Int, lambdaParamCounter::Int}

buildInputTreeForValueUnderConstruction :: UI.UntypedValueUnderConstruction -> State InputTreeState I.InputExpression
buildInputTreeForValueUnderConstruction (UI.UntypedValueUnderConstruction valueType valueDefinition) = do
    inputType <- uiToInputType $ Just valueType
    inputValueDefinition <- buildInputTree valueDefinition
    return $ I.InputValueDefinition inputType inputValueDefinition

buildInputTree ::  UI.UntypedValue -> State InputTreeState I.InputExpression
buildInputTree e = case e of
    UI.TypeHole -> return $ I.InputTypeHole I.InputUnknownType
    UI.Lambda typ lambdaValue -> do
        s <- get
        let paramName = [chr (ord 'i' + lambdaParamCounter s)]
        put $ s{lambdaParamCounter = lambdaParamCounter s + 1}
        inputType <- uiToInputType typ
        case lambdaValue of
            UI.LambdaValue inner -> do
                _inner <- buildInputTree inner
                return $ I.InputLambda inputType paramName _inner
            UI.ValueToFill -> return $ I.InputLambda inputType paramName $ I.InputTypeHole I.InputUnknownType
    UI.Reference typ name args -> do
        inputType <- uiToInputType typ
        let constant = I.InputReference inputType name
        _s <- get
        case args of
            UI.ArgumentList _args ->
                foldM (\inner arg -> do
                    argInput <- buildInputTree arg
                    return $ I.InputApplication I.InputUnknownType inner argInput)
                    constant _args
            UI.ToFill toFill -> do
                inputToFill <- uiToInputType (Just toFill)
                let inputCardinality = countInputCardinality inputType
                    toFillCardinality = countUICardinality toFill
                inner <- if inputCardinality <= toFillCardinality
                    then return constant
                    else do
                        let nums = [1..inputCardinality - toFillCardinality ]
                        applied <- foldM (\inner _ -> do
                            return $ I.InputApplication I.InputUnknownType inner $ I.InputTypeHole I.InputUnknownType)
                            constant nums
                        case applied of
                            I.InputApplication _ inner th -> return $ I.InputApplication inputToFill inner th
                            _ -> return applied
                return $ I.InputValueDefinition inputToFill inner
            UI.UnknownArgs -> error "cannot deal with unknown args"
    UI.IntegerLiteral value -> case value of
        Nothing -> literalError
        Just lit -> return $ I.InputLiteral (I.InputPrimitive "int") (filter (/= '"') $ show lit)
    UI.StringLiteral value -> case value of
        Nothing -> literalError
        Just lit -> return $ I.InputLiteral (I.InputPrimitive "string") lit

    where
        literalError = error "literals must be filled in the UI layer"

        countInputCardinality :: I.InputType -> Int
        countInputCardinality (I.InputFunction _ to) = 1 + countInputCardinality to
        countInputCardinality _ = 0

        countUICardinality :: UI.Type -> Int
        countUICardinality (UI.Function _ to) = 1 + countUICardinality to
        countUICardinality _ = 0

getMaxGeneric :: Maybe UI.Type -> Int
getMaxGeneric uiType = case uiType of
    Nothing -> 0
    Just (UI.Primitive _) -> 0
    Just (UI.Function from to) -> max (getMaxGeneric (Just from)) (getMaxGeneric (Just to))
    Just (UI.List item) -> getMaxGeneric $ Just item
    Just (UI.Generic num) -> num

uiToInputType :: Maybe UI.Type -> State InputTreeState I.InputType
uiToInputType typ = do
        s <- get
        let currentOffset = genericCounter s
        let inputType = _uiToInputType currentOffset typ
        let newOffset =  currentOffset + getMaxGeneric typ
        put $ s{genericCounter = newOffset}
        return inputType
    where
        _uiToInputType :: Int -> Maybe UI.Type -> I.InputType
        _uiToInputType genericOffset uiType = case uiType of 
            Nothing -> I.InputUnknownType
            Just (UI.Primitive n) -> I.InputPrimitive n
            Just (UI.Function from to) -> I.InputFunction
                (_uiToInputType genericOffset $ Just from)
                (_uiToInputType genericOffset $ Just to)
            Just (UI.List item) -> I.InputList (_uiToInputType genericOffset $ Just item)
            Just (UI.Generic num) -> I.InputGeneric $ genericOffset + num

buildOutputTree :: O.InferedExpression -> UI.TypedValue
buildOutputTree ex = case ex of
    O.InferedTypeHole name typ -> UI.TypedTypeHole (inferedToUIType typ) name
    O.InferedReference name typ -> UI.TypedReference (inferedToUIType typ) name []
    O.InferedLiteral name typ -> UI.TypedLiteral (inferedToUIType typ) name
    O.InferedApplication func arg _ ->
        let uiFunc = buildOutputTree func
            uiArg = buildOutputTree arg in
        case uiFunc of
            UI.TypedReference rTyp rName rArgs -> UI.TypedReference rTyp rName $ rArgs ++ [uiArg]
            _ -> error "Can only apply to references"
    O.InferedLambda (paramName, paramType) inner typ ->
        UI.TypedLambda (inferedToUIType typ) (inferedToUIType paramType, paramName) $ buildOutputTree inner
    O.InferedTuple {} -> error "Tuples are not supported in the UI model"
    where
        inferedToUIType :: O.InferedType -> UI.Type
        inferedToUIType (O.InferedConstantType name) = UI.Primitive name
        inferedToUIType (O.InferedGeneric num) = UI.Generic num
        inferedToUIType (O.InferedFunctionType from to) = UI.Function (inferedToUIType from) (inferedToUIType to)
        inferedToUIType (O.InferedListType item) = UI.List (inferedToUIType item)
        inferedToUIType (O.InferedTupleType _ _) = error "Tuples are not supported in the UI model"

infere :: UI.UntypedValueUnderConstruction -> UI.InferenceResult
infere valueUnderConstruction =
    let input = evalState (buildInputTreeForValueUnderConstruction (trace ("Untyped: " ++ show valueUnderConstruction) valueUnderConstruction)) InputTreeState{genericCounter=0,lambdaParamCounter=0}
        (elaboratedExpression, typeConstraints) = elaboration (trace ("Input: " ++ show input) input)
        unifcationResult = unification (trace ("Constraints: " ++ show typeConstraints) typeConstraints)
        zonked = zonking (trace ("ElaboratedExpression: " ++ show elaboratedExpression) elaboratedExpression) unifcationResult
    in case trace ("Zonked: " ++ show zonked) zonked of
        Left e -> UI.Error e
        Right r -> UI.Success $ buildOutputTree r
