module VFP.Inference.Zonking where

import VFP.Inference.Elaboration ( ElaboratedExpression(..) )
import VFP.Inference.Unification
import Debug.Trace
import qualified Data.Map.Strict as Map
import Control.Monad.State.Lazy

data InferedType = InferedConstantType String
                 | InferedTupleType InferedType InferedType
                 | InferedFunctionType InferedType InferedType
                 | InferedListType InferedType
                 | InferedGeneric Int
                 deriving (Eq)

data InferedExpression = InferedConstant String InferedType
                       | InferedApplication InferedExpression InferedExpression InferedType
                       | InferedTuple InferedExpression InferedExpression InferedType
                       | InferedLambda  (String, InferedType) InferedExpression InferedType
                       | InferedTypeHole String InferedType
                       deriving (Eq)

instance Show InferedType where
    show (InferedConstantType name) = name
    show (InferedGeneric num) = show num
    show (InferedTupleType l r) = "(" ++ show l ++ "," ++ show r ++ ")"
    show (InferedListType i) = "[" ++ show i ++ "]"
    show (InferedFunctionType from to) = show from ++ " -> " ++ show to

instance Show InferedExpression where
    show (InferedConstant name t) = "(" ++ name ++ ":" ++ show t ++ ")"
    show (InferedApplication left right t) = "(" ++ show left ++ " " ++ show right ++ "):" ++ show t
    show (InferedTuple left right t) = "(" ++ show left ++ "," ++ show right ++ "):" ++ show t
    show (InferedLambda (variableName, variableType) nested t) = "λ" ++ variableName ++ ":" ++ show variableType  ++ "." ++ show nested ++ ":" ++ show t
    show (InferedTypeHole name typ) = name ++ ":" ++ show typ

type InferenceResult = Either String InferedExpression


data ZonkingState = ZonkingState { resolveds :: ResolvedTypes, generics :: Map.Map UnificationType Int }

getOrCreateResolvedGeneric :: UnificationType -> State ZonkingState InferedType
getOrCreateResolvedGeneric typ = do
    s <- get
    case Map.lookup typ $ generics s of
        Just generic -> return $ InferedGeneric generic 
        Nothing -> do
            let next = (if Map.null $ generics s then 0 else maximum $ Map.elems $ generics s) + 1
            let inferredGeneric = InferedGeneric next
            put $ s{generics = Map.insert typ next $ generics s}
            return inferredGeneric

resolveType :: UnificationType -> State ZonkingState InferedType
resolveType (UnificationVariable var isGeneric) = do
    s <- get 
    case Map.lookup (UnificationVariable var isGeneric) $ resolveds s of
        Nothing -> getOrCreateResolvedGeneric (UnificationVariable var isGeneric)
        Just x -> resolveType x
resolveType (UnificationConstructedType "(,)" [l, r]) = do
    left <- resolveType l
    right <- resolveType r
    return $ InferedTupleType left right
resolveType (UnificationConstructedType "->" [from, to]) = do
    inferedFrom <- resolveType from
    inferedTo <- resolveType to
    return $ InferedFunctionType inferedFrom inferedTo
resolveType (UnificationConstructedType "[]" [item]) = do
    inferedItem <- resolveType item
    return $ InferedListType inferedItem
resolveType (UnificationConstructedType _ _) = error "Constructed Type not supported"
resolveType (UnificationConstantType name) = return $ InferedConstantType name

zonk :: ElaboratedExpression -> State ZonkingState InferenceResult
zonk expr = case expr of
    ElaboratedConstant typ name ->  do
        infered <- resolveType typ
        return $ Right $ InferedConstant name infered
    ElaboratedTypeHole typ name -> do
        infered <- resolveType typ
        return $ Right $ InferedTypeHole name infered
    ElaboratedApplication typ left right -> do
        infered <- resolveType typ
        l <- zonk left
        r <- zonk right
        return $ case l of
            Left e -> Left e
            Right inferedL -> case r of
                Left e -> Left e
                Right inferedR -> Right $ InferedApplication inferedL inferedR infered
    ElaboratedLambda typ (variableName, variableType) nested -> do
        infered <- resolveType typ
        inferedVar <- resolveType variableType
        nestedEx <- zonk nested
        return $ case nestedEx of
            Left e -> Left e
            Right inferedEx -> Right $ InferedLambda (variableName, inferedVar) inferedEx infered
    ElaboratedTuple typ left right -> do
        infered <- resolveType typ
        l <- zonk left
        r <- zonk right
        return $ case l of
            Left e -> Left e
            Right inferedL -> case r of
                Left e -> Left e
                Right inferedR -> Right $ InferedTuple inferedL inferedR infered

checkResiduals :: (TypeConstraintConjunction, ResolvedTypes) -> Either String ZonkingState
checkResiduals (residuals, types) = trace ("Residuals: " ++ show residuals) $ trace ("ResolvedTypes: " ++ show types) $ do
    if not $ all (\(l,r) -> typeContainsVariable l && typeContainsVariable r) residuals then
        Left "Expression could not be solved"
    else 
        Right $ ZonkingState {resolveds = types, generics = Map.empty} 

zonking :: ElaboratedExpression -> (TypeConstraintConjunction, ResolvedTypes) -> InferenceResult
zonking ex unificationResult = do
    s <- checkResiduals $ trace ("ElaboratedExpression: " ++ show ex) unificationResult
    evalState (zonk ex) s 