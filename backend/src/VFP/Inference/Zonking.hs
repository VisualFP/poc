-- Copyright (C) 2023 Lukas Streckeisen & Jann Flepp

module VFP.Inference.Zonking where

import VFP.Inference.Elaboration ( ElaboratedExpression(..) )
import VFP.Inference.Unification
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

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
            ElaboratedLambda _ (variableName, _) nested ->
                _checkScopes nested (filter (/= variableName) bannedIdentifiers) 
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

zonking :: ElaboratedExpression -> (TypeConstraintConjunction, ResolvedTypes) -> InferenceResult
zonking expr (residuals, types) = do
    checkResiduals residuals
    checkScopes expr
    zonk expr types
