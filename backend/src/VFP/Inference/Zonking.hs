-- Copyright (C) 2023 Lukas Streckeisen & Jann Flepp

module VFP.Inference.Zonking where

import VFP.Inference.Elaboration ( ElaboratedExpression(..) )
import VFP.Inference.Unification
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Control.Monad.State.Strict

data InferedType = InferedConstantType String
                 | InferedTupleType InferedType InferedType
                 | InferedFunctionType InferedType InferedType
                 | InferedListType InferedType
                 | InferedGeneric Int
                 deriving (Eq)

data InferedExpression = InferedValueDefinition String InferedType InferedExpression
                       | InferedReference String InferedType
                       | InferedApplication InferedExpression InferedExpression InferedType
                       | InferedTuple InferedExpression InferedExpression InferedType
                       | InferedLambda  (String, InferedType) InferedExpression InferedType
                       | InferedTypeHole String InferedType
                       | InferedLiteral String InferedType
                       deriving (Eq)

instance Show InferedType where
    show (InferedConstantType name) = name
    show (InferedGeneric num) = show num
    show (InferedTupleType l r) = "(" ++ show l ++ "," ++ show r ++ ")"
    show (InferedListType i) = "[" ++ show i ++ "]"
    show (InferedFunctionType from to) = show from ++ " -> " ++ show to

instance Show InferedExpression where
    show (InferedValueDefinition name t inner) = "(" ++ show inner ++ "-" ++ show name ++ ":" ++ show t ++ ")"
    show (InferedReference name t) = "(" ++ name ++ ":" ++ show t ++ ")"
    show (InferedLiteral name t) = "(" ++ name ++ ":" ++ show t ++ ")"
    show (InferedApplication left right t) = "(" ++ show left ++ " " ++ show right ++ "):" ++ show t
    show (InferedTuple left right t) = "(" ++ show left ++ "," ++ show right ++ "):" ++ show t
    show (InferedLambda (variableName, variableType) nested t) = "λ" ++ variableName ++ ":" ++ show variableType  ++ "." ++ show nested ++ ":" ++ show t
    show (InferedTypeHole name typ) = name ++ ":" ++ show typ

type InferenceResult = Either String InferedExpression

checkResiduals :: TypeConstraintConjunction -> Either String ()
checkResiduals residuals =
    if Set.null residuals then Right () else
        let (l, r) = Set.elemAt 0 residuals in
        Left $ "Inference failed: Could not unify \"" ++ show l ++ " ~ " ++ show r ++ "\""

checkScopes :: ElaboratedExpression -> Either String ()
checkScopes ex =
        let allIdentifiers = getDeclaredIdentifiers ex in
        _checkScopes ex allIdentifiers
    where
        getDeclaredIdentifiers :: ElaboratedExpression -> [String]
        getDeclaredIdentifiers curEx = case curEx of
            ElaboratedReference _ _ -> []
            ElaboratedLiteral _ _ -> []
            ElaboratedTypeHole _ _ -> []
            ElaboratedValueDefinition _ name inner -> name : getDeclaredIdentifiers inner
            ElaboratedApplication _ left right -> getDeclaredIdentifiers left ++ getDeclaredIdentifiers right
            ElaboratedLambda _ (variableName, _) nested -> variableName : getDeclaredIdentifiers nested
            ElaboratedTuple _ left right -> getDeclaredIdentifiers left ++ getDeclaredIdentifiers right

        _checkScopes :: ElaboratedExpression -> [String] -> Either String ()
        _checkScopes curEx bannedIdentifiers = case curEx of
            ElaboratedReference _ name -> if name `elem` bannedIdentifiers
                then Left $ "Scope violation: \"" ++ name ++ "\" cannot be used here"
                else Right ()
            ElaboratedApplication _ left right -> do
                _checkScopes left bannedIdentifiers
                _checkScopes right bannedIdentifiers
            ElaboratedTuple _ left right -> do
                _checkScopes left bannedIdentifiers
                _checkScopes right bannedIdentifiers
            ElaboratedLambda _ (variableName, _) nested ->
                _checkScopes nested (filter (/= variableName) bannedIdentifiers)
            ElaboratedValueDefinition _ name nested ->
                _checkScopes nested (filter (/= name) bannedIdentifiers)
            _ -> Right ()

resolveType :: UnificationType -> ResolvedTypes -> InferedType
resolveType typ resolvedTypes = case typ of
    UnificationVariable var isGeneric ->
        case Map.lookup (UnificationVariable var isGeneric) resolvedTypes of
            Nothing -> error $ "variable " ++ show var ++ " not resolved"
            Just x -> resolveType x resolvedTypes
    UnificationConstructedType "(,)" [l, r] ->
        InferedTupleType (resolveType l resolvedTypes) (resolveType r resolvedTypes)
    UnificationConstructedType "[]" [item] ->
        InferedListType $ resolveType item resolvedTypes
    UnificationConstructedType "->" [from, to] ->
        InferedFunctionType (resolveType from resolvedTypes) (resolveType to resolvedTypes)
    UnificationConstructedType _ _ -> error "Constructed Type not supported"
    UnificationConstantType ('G':num) ->
        InferedGeneric (read num :: Int)
    UnificationConstantType name ->
        InferedConstantType name

zonk :: ElaboratedExpression -> ResolvedTypes -> InferenceResult
zonk expr types = case expr of
    ElaboratedValueDefinition typ name inner -> do
        innerExpression <- zonk inner types
        return $ InferedValueDefinition name (resolveType typ types) innerExpression
    ElaboratedReference typ name -> Right $ InferedReference name $ resolveType typ types
    ElaboratedLiteral typ name -> Right $ InferedLiteral name $ resolveType typ types
    ElaboratedTypeHole typ name -> Right $ InferedTypeHole name $ resolveType typ types
    ElaboratedApplication typ left right -> do
        l <- zonk left types
        r <- zonk right types
        Right $ InferedApplication l r $ resolveType typ types
    ElaboratedLambda typ (variableName, variableType) nested -> do
        nestedEx <- zonk nested types
        Right $ InferedLambda (variableName, resolveType variableType types) nestedEx (resolveType typ types)
    ElaboratedTuple typ left right -> do
        leftType <- zonk left types
        rightType <- zonk right types
        Right $ InferedTuple leftType rightType $ resolveType typ types

normalizeGenerics :: InferedExpression -> InferedExpression
normalizeGenerics e = evalState (_normalizeGenericsExpression e) Map.empty
    where
        _normalizeGenericsExpression :: InferedExpression -> State (Map.Map Int Int) InferedExpression
        _normalizeGenericsExpression (InferedValueDefinition name typ inner) = do
            newTyp <- _normalizeGenericsType typ
            newInner <- _normalizeGenericsExpression inner
            return $ InferedValueDefinition name newTyp newInner
        _normalizeGenericsExpression (InferedReference name typ) = do
            newTyp <- _normalizeGenericsType typ
            return $ InferedReference name newTyp
        _normalizeGenericsExpression (InferedApplication left right typ) = do
            newTyp <- _normalizeGenericsType typ
            newLeft <- _normalizeGenericsExpression left
            newRight <- _normalizeGenericsExpression right
            return $ InferedApplication newLeft newRight newTyp
        _normalizeGenericsExpression (InferedTuple left right typ) = do
            newTyp <- _normalizeGenericsType typ
            newLeft <- _normalizeGenericsExpression left
            newRight <- _normalizeGenericsExpression right
            return $ InferedTuple newLeft newRight newTyp
        _normalizeGenericsExpression (InferedLambda (pName, pTyp) inner typ) = do
            newTyp <- _normalizeGenericsType typ
            newPTyp <- _normalizeGenericsType pTyp
            newInner <- _normalizeGenericsExpression inner
            return $ InferedLambda (pName, newPTyp) newInner newTyp
        _normalizeGenericsExpression (InferedTypeHole name typ) = do
            newTyp <- _normalizeGenericsType typ
            return $ InferedTypeHole name newTyp
        _normalizeGenericsExpression (InferedLiteral name typ) = return $ InferedLiteral name typ

        _normalizeGenericsType :: InferedType -> State (Map.Map Int Int) InferedType
        _normalizeGenericsType (InferedConstantType name) = return $ InferedConstantType name
        _normalizeGenericsType (InferedTupleType left right) = do
            newLeft <- _normalizeGenericsType left
            newRight <- _normalizeGenericsType right
            return $ InferedTupleType newLeft newRight
        _normalizeGenericsType (InferedFunctionType param expr) = do
            newParam <- _normalizeGenericsType param
            newExpr <- _normalizeGenericsType expr
            return $ InferedFunctionType newParam newExpr
        _normalizeGenericsType (InferedListType item) = do
            newItem <- _normalizeGenericsType item
            return $ InferedListType newItem
        _normalizeGenericsType (InferedGeneric counter) = do
            current <- get
            case Map.lookup counter current of
                Nothing -> do
                    let next = maximum (Map.elems current ++ [0]) + 1
                    put $ Map.insert counter next current
                    return $ InferedGeneric next
                Just c -> return $ InferedGeneric c

zonking :: ElaboratedExpression -> (TypeConstraintConjunction, ResolvedTypes) -> InferenceResult
zonking expr (residuals, types) = do
    checkResiduals residuals
    checkScopes expr
    case zonk expr types of
        Left e -> Left e
        Right r -> Right $ normalizeGenerics r
