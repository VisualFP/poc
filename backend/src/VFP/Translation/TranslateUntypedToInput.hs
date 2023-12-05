module VFP.Translation.TranslateUntypedToInput(translateUntypedToInput) where

import qualified VFP.UI.UIModel as UI
import qualified VFP.Inference.InputModel as I
import Control.Monad.State.Strict
import Control.Monad (foldM)
import Data.Char

getMaxGeneric :: Maybe UI.Type -> Int
getMaxGeneric uiType = case uiType of
    Nothing -> 0
    Just (UI.Primitive _) -> 0
    Just (UI.Function from to) -> max (getMaxGeneric (Just from)) (getMaxGeneric (Just to))
    Just (UI.List item) -> getMaxGeneric $ Just item
    Just (UI.Generic num) -> num

countUITypeCardinality :: UI.Type -> Int
countUITypeCardinality (UI.Function _ to) = 1 + countUITypeCardinality to
countUITypeCardinality _ = 0

countInputTypeCardinality :: I.InputType -> Int
countInputTypeCardinality (I.InputFunction _ to) = 1 + countInputTypeCardinality to
countInputTypeCardinality _ = 0

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

data InputTreeState = InputTreeState {genericCounter::Int, lambdaParamCounter::Int}

buildInputTree ::  UI.UntypedValue -> State InputTreeState I.InputExpression
buildInputTree e = case e of
    UI.TypeHole -> return $ I.InputTypeHole I.InputUnknownType
    UI.ValueDefinition typ name inner -> do
        inputType <- uiToInputType typ
        inputInner <- buildInputTree inner 
        return $ I.InputValueDefinition inputType name inputInner
    UI.Lambda typ inner -> do
        s <- get
        let paramName = [chr (ord 'i' + lambdaParamCounter s)]
        put $ s{lambdaParamCounter = lambdaParamCounter s + 1}
        inputType <- uiToInputType typ
        _inner <- buildInputTree inner
        return $ I.InputLambda inputType paramName _inner
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
                let inputCardinality = countInputTypeCardinality inputType
                    toFillCardinality = countUITypeCardinality toFill
                inner <- if inputCardinality <= toFillCardinality
                    then return constant
                    else do
                        let nums = [1..inputCardinality - toFillCardinality]
                        applied <- foldM (\inner _ -> do
                            return $ I.InputApplication I.InputUnknownType inner $ I.InputTypeHole I.InputUnknownType)
                            constant nums
                        case applied of
                            I.InputApplication _ inner th -> return $ I.InputApplication inputToFill inner th
                            _ -> return applied
                return $ I.InputValueConstraint inputToFill inner
            UI.UnknownArgs -> error "cannot deal with unknown args"
    UI.IntegerLiteral value -> case value of
        Nothing -> literalError
        Just lit -> return $ I.InputLiteral (I.InputPrimitive "Int") (filter (/= '"') $ show lit)
    UI.BooleanLiteral value -> return $ I.InputLiteral (I.InputPrimitive "Bool") value
    UI.StringLiteral value -> case value of
        Nothing -> literalError
        Just lit -> return $ I.InputLiteral (I.InputPrimitive "String") lit
    where
        literalError = error "literals must be filled in the UI layer"


translateUntypedToInput :: UI.UntypedValue -> I.InputExpression
translateUntypedToInput input = evalState (buildInputTree input) InputTreeState{genericCounter=0,lambdaParamCounter=0}