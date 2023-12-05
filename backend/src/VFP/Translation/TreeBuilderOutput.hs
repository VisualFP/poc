module VFP.Translation.TreeBuilderOutput(buildOutputTree) where

import qualified VFP.UI.UIModel as UI
import qualified VFP.Inference.Zonking as O

inferedToUIType :: O.InferedType -> UI.Type
inferedToUIType (O.InferedConstantType name) = UI.Primitive name
inferedToUIType (O.InferedGeneric num) = UI.Generic num
inferedToUIType (O.InferedFunctionType from to) = UI.Function (inferedToUIType from) (inferedToUIType to)
inferedToUIType (O.InferedListType item) = UI.List (inferedToUIType item)
inferedToUIType (O.InferedTupleType _ _) = error "Tuples are not supported in the UI model"

buildOutputTree :: O.InferedExpression -> UI.TypedValue
buildOutputTree ex = case ex of
    O.InferedValueDefinition name typ inner -> UI.TypedValueDefinition (inferedToUIType typ) name $ buildOutputTree inner
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
