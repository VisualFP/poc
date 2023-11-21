module VFP.Translation.InferenceTranslation where

import qualified VFP.UI.UIModel as UI
import qualified VFP.Translation.WellKnown as WellKnown
import qualified VFP.Inference.InputModel as I
import qualified VFP.Inference.Zonking as O
import VFP.Inference.Elaboration (elaboration)
import VFP.Inference.Unification (unification)
import VFP.Inference.Zonking (zonking)

buildInputTree :: UI.UntypedValue -> I.InputExpression
buildInputTree (UI.Reference name arguments) =
    let input = WellKnown.lookupWellKnownInputConstant name in
    case arguments of
    UI.Unknown ->
        case I.getInputType input of
            (I.InputFunction func arg) -> I.InputApplication I.InputUnknownType input (I.InputConstant I.InputUnknownType "")
            _ -> input
        where
            buildInputFunctionWithArgs :: String -> I.InputType -> I.InputExpression
            buildInputFunctionWithArgs name (I.InputFunction argType resType) =
                let nestedExpr = buildInputFunctionWithArgs name resType
                in I.InputApplication (I.getInputType nestedExpr) nestedExpr (I.InputConstant argType "?")
            buildInputFunctionWithArgs name t = I.InputConstant t name 
    UI.ArgumentList args ->
        foldr (\arg expression -> I.InputApplication I.InputUnknownType expression (buildInputTree arg) ) input args

buildOutputTree :: O.InferedExpression -> UI.TypedValue
buildOutputTree ex = case ex of
    O.InferedConstant name typ -> UI.TypedReference (oToUIType typ) name []
    O.InferedLambda (pName, pTyp) subEx typ -> UI.TypedLambda (oToUIType typ) (pName, oToUIType pTyp) (buildOutputTree subEx)
    O.InferedApplication left right typ ->
        let inner = buildOutputTree left in
        case inner of
            UI.TypedReference rTyp rId rValues -> UI.TypedReference rTyp rId (buildOutputTree right:rValues)
            _ -> error "You only apply to references"
    where
        oToUIType :: O.InferedType -> UI.Type
        oToUIType = show

infere :: UI.UntypedValue -> UI.InferenceResult
infere untyped =
    let input = buildInputTree untyped
        (elaboratedExpression, typeConstraints) = elaboration input
        unifcationResult = unification typeConstraints
        zonked = zonking elaboratedExpression unifcationResult
    in case zonked of
        Left error -> UI.Error error
        Right r -> UI.Success $ buildOutputTree r
