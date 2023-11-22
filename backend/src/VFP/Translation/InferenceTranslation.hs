{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module VFP.Translation.InferenceTranslation where

import qualified VFP.UI.UIModel as UI
import qualified VFP.Translation.WellKnown as WellKnown
import qualified VFP.Inference.InputModel as I
import qualified VFP.Inference.Zonking as O
import VFP.Inference.Elaboration (elaboration)
import VFP.Inference.Unification (unification)
import VFP.Inference.Zonking (zonking)
import Control.Monad.State.Lazy
import VFP.Inference.InputModel (InputType(InputUnknownType))
import Control.Monad (foldM)
import Data.List

buildInputTree ::  UI.UntypedValue -> State Int I.InputExpression
buildInputTree e = do 
    case e of
        UI.TypeHole -> getNextTypeHole
        UI.Lambda name inner -> do
            inner <- buildInputTree inner
            return $ I.InputLambda I.InputUnknownType name inner
        UI.Reference typ name args ->
            let constant = I.InputConstant (uiToInputType typ) name in
            case args of
                UI.ArgumentList args ->
                    foldM (\inner arg -> do
                        argInput <- buildInputTree arg
                        return $ I.InputApplication I.InputUnknownType inner argInput)
                        constant args
                UI.ToFill toFill ->
                    let inputCardinality = countInputCardinality $ uiToInputType typ
                        toFillCardinality = countUICardinality toFill in
                    if inputCardinality <= toFillCardinality then return constant
                    else do
                        let nums = [1..inputCardinality - toFillCardinality ]
                        foldM (\inner _ -> do
                            th <- getNextTypeHole
                            return $ I.InputApplication I.InputUnknownType inner th)
                            constant nums
    where
        countInputCardinality :: I.InputType -> Int
        countInputCardinality (I.InputFunction from to) = 1 + countInputCardinality to
        countInputCardinality _ = 0

        countUICardinality :: UI.Type -> Int
        countUICardinality (UI.Function from to) = 1 + countUICardinality to
        countUICardinality _ = 0

        uiToInputType :: Maybe UI.Type -> I.InputType
        uiToInputType (Just (UI.Primitive n)) = I.InputPrimitive n
        uiToInputType (Just UI.UnknownType) = I.InputUnknownType
        uiToInputType (Just (UI.Function from to)) = I.InputFunction (uiToInputType $ Just from) (uiToInputType $ Just to)
        uiToInputType Nothing = InputUnknownType

        getNextTypeHole :: State Int I.InputExpression
        getNextTypeHole = do
            current <- get
            put $ current + 1
            return $ I.InputConstant I.InputUnknownType ("TH" ++ show current)

buildOutputTree :: O.InferedExpression -> UI.TypedValue
buildOutputTree ex = case ex of
    O.InferedConstant name typ ->
        if isPrefixOf "TH" name then UI.TypedTypeHole (inferedToUIType typ) name
        else UI.TypedReference (inferedToUIType typ) name []
    O.InferedApplication func arg typ ->
        let uiFunc = buildOutputTree func
            uiArg = buildOutputTree arg in
        case uiFunc of
            UI.TypedReference rTyp rName rArgs -> UI.TypedReference rTyp rName $ uiArg:rArgs
            _ -> error "Can only apply to references"
    O.InferedLambda (paramName, paramType) inner typ ->
        UI.TypedLambda (inferedToUIType typ) (inferedToUIType paramType, paramName) $ buildOutputTree inner
    O.InferedTuple {} -> error "Tuples are not supported in the UI model"
    where
        inferedToUIType :: O.InferedType -> UI.Type
        inferedToUIType (O.InferedConstantType name) = UI.Primitive name
        inferedToUIType (O.InferedFunctionType from to) = UI.Function (inferedToUIType from) (inferedToUIType to)
        inferedToUIType (O.InferedTupleType _ _) = error "Tuples are not supported in the UI model"

infere :: UI.UntypedValue -> UI.InferenceResult
infere untyped =
    let input = evalState (buildInputTree untyped) 1
        (elaboratedExpression, typeConstraints) = elaboration input
        unifcationResult = unification typeConstraints
        zonked = zonking elaboratedExpression unifcationResult
    in case zonked of
        Left error -> UI.Error error
        Right r -> UI.Success $ buildOutputTree r
