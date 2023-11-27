{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module VFP.Translation.InferenceTranslation where

import qualified VFP.UI.UIModel as UI
import qualified VFP.Inference.InputModel as I
import qualified VFP.Inference.Zonking as O
import VFP.Inference.Elaboration (elaboration)
import VFP.Inference.Unification (unification)
import VFP.Inference.Zonking (zonking)
import Control.Monad.State.Lazy
import Control.Monad (foldM)

buildInputTree ::  UI.UntypedValue -> State Int I.InputExpression
buildInputTree e = do 
    case e of
        UI.TypeHole -> return $ I.InputTypeHole I.InputUnknownType
        UI.Lambda typ name lambdaValue -> do
            case lambdaValue of
                UI.LambdaValue inner -> do
                    _inner <- buildInputTree inner
                    return $ I.InputValueDefinition (uiToInputType typ) (I.InputLambda I.InputUnknownType name _inner)
                UI.ValueToFill -> return $ I.InputValueDefinition (uiToInputType typ) (I.InputLambda I.InputUnknownType name $ I.InputTypeHole I.InputUnknownType)
        UI.Reference typ name args ->
            let constant = I.InputConstant (uiToInputType typ) name in
            case args of
                UI.ArgumentList _args ->
                    foldM (\inner arg -> do
                        argInput <- buildInputTree arg
                        return $ I.InputApplication I.InputUnknownType inner argInput)
                        constant _args
                UI.ToFill toFill -> do
                    let inputCardinality = countInputCardinality $ uiToInputType typ
                        toFillCardinality = countUICardinality toFill
                    inner <- if inputCardinality <= toFillCardinality
                        then return constant
                        else do
                            let nums = [1..inputCardinality - toFillCardinality ]
                            applied <- foldM (\inner _ -> do
                                return $ I.InputApplication I.InputUnknownType inner $ I.InputTypeHole I.InputUnknownType)
                                constant nums
                            case applied of
                                I.InputApplication _ inner th -> return $ I.InputApplication (uiToInputType (Just toFill)) inner th
                                _ -> return applied
                    return $ I.InputValueDefinition (uiToInputType $ Just toFill) inner
    where
        countInputCardinality :: I.InputType -> Int
        countInputCardinality (I.InputFunction _ to) = 1 + countInputCardinality to
        countInputCardinality _ = 0

        countUICardinality :: UI.Type -> Int
        countUICardinality (UI.Function _ to) = 1 + countUICardinality to
        countUICardinality _ = 0

        uiToInputType :: Maybe UI.Type -> I.InputType
        uiToInputType (Just (UI.Primitive n)) = I.InputPrimitive n
        uiToInputType (Just (UI.Function from to)) = I.InputFunction (uiToInputType $ Just from) (uiToInputType $ Just to)
        uiToInputType (Just (UI.List item)) = I.InputList (uiToInputType $ Just item)
        uiToInputType (Just (UI.Generic num)) = I.InputGeneric num
        uiToInputType Nothing = I.InputUnknownType

buildOutputTree :: O.InferedExpression -> UI.TypedValue
buildOutputTree ex = case ex of
    O.InferedTypeHole name typ -> UI.TypedTypeHole (inferedToUIType typ) name
    O.InferedConstant name typ -> UI.TypedReference (inferedToUIType typ) name []
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

infere :: UI.UntypedValue -> UI.InferenceResult
infere untyped =
    let input = evalState (buildInputTree untyped) 1
        (elaboratedExpression, typeConstraints) = elaboration input
        unifcationResult = unification typeConstraints
        zonked = zonking elaboratedExpression unifcationResult
    in case zonked of
        Left e -> UI.Error e
        Right r -> UI.Success $ buildOutputTree r
