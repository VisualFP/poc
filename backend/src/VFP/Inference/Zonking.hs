module VFP.Inference.Zonking where

import VFP.Inference.Elaboration ( ElaboratedExpression(..) )
import VFP.Inference.Unification
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data InferedType = InferedConstantType String
                 | InferedConstructedType String [InferedType]
                 deriving (Eq)

data InferedExpression = InferedConstant String InferedType
                       | InferedApplication InferedExpression InferedExpression InferedType
                       | InferedTuple InferedExpression InferedExpression InferedType
                       | InferedLambda  (String, InferedType) InferedExpression InferedType
                       deriving (Eq)

instance Show InferedType where
    show (InferedConstantType name) = name
    show (InferedConstructedType name types) = name ++ "," ++ intercalate "," (map show types)

instance Show InferedExpression where
    show (InferedConstant name t) = "(" ++ name ++ ":" ++ show t ++ ")"
    show (InferedApplication left right t) = "(" ++ show left ++ " " ++ show right ++ "):" ++ show t
    show (InferedTuple left right t) = "(" ++ show left ++ "," ++ show right ++ "):" ++ show t
    show (InferedLambda (variableName, variableType) nested t) = "λ" ++ variableName ++ ":" ++ show variableType  ++ "." ++ show nested ++ ":" ++ show t

type InferenceResult = Either String InferedExpression 

zonking :: ElaboratedExpression -> (TypeConstraintConjunction, ResolvedTypes) -> InferenceResult
zonking expr (residuals, types) = if not $ Set.null residuals then Left "Expression could not be solved" else
    case expr of
        ElaboratedConstant typ name -> Right $ InferedConstant name $ resolveType typ
        ElaboratedApplication typ left right -> do
            l <- zonking left (residuals, types)
            r <- zonking right (residuals, types)
            Right $ InferedApplication l r $ resolveType typ
        ElaboratedLambda typ (variableName, variableType) nested -> do
            nestedEx <- zonking nested (residuals, types)
            Right $ InferedLambda (variableName, resolveType variableType) nestedEx (resolveType typ)
        ElaboratedTuple typ left right -> do
            leftType <- zonking left (residuals, types)
            rightType <- zonking right (residuals, types)
            Right $ InferedTuple leftType rightType $ resolveType typ
    where
        resolveType :: UnificationType -> InferedType
        resolveType (UnificationVariable var) = case Map.lookup (UnificationVariable var) types of
            Nothing -> error $ "variable " ++ show var ++ " not resolved"
            Just x -> resolveType x
        resolveType (UnificationConstructedType name ts) = InferedConstructedType name $ map resolveType ts
        resolveType (UnificationConstantType name) = InferedConstantType name


