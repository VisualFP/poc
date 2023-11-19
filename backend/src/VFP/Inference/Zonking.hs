module VFP.Inference.Zonking where

import VFP.Inference.Elaboration ( ElaboratedExpression(..) )
import VFP.Inference.Unification
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data InferredType = InferredConstantType String
                  | InferredConstructedType String [InferredType]
                  deriving (Eq)

data InferredExpression = InferredConstant String InferredType
                        | InferredApplication InferredExpression InferredExpression InferredType
                        | InferredTuple InferredExpression InferredExpression InferredType
                        | InferredLambda  (String, InferredType) InferredExpression InferredType
                        deriving (Eq)

instance Show InferredType where
    show (InferredConstantType name) = name
    show (InferredConstructedType name types) = name ++ "," ++ intercalate "," (map show types)

instance Show InferredExpression where
    show (InferredConstant name t) = "(" ++ name ++ ":" ++ show t ++ ")"
    show (InferredApplication left right t) = "(" ++ show left ++ " " ++ show right ++ "):" ++ show t
    show (InferredTuple left right t) = "(" ++ show left ++ "," ++ show right ++ "):" ++ show t
    show (InferredLambda (variableName, variableType) nested t) = "λ" ++ variableName ++ ":" ++ show variableType  ++ "." ++ show nested ++ ":" ++ show t

type InferrenceResult = Either String InferredExpression 

zonking :: ElaboratedExpression -> (TypeConstraintConjunction, ResolvedTypes) -> InferrenceResult
zonking expr (residuals, types) = if not $ Set.null residuals then Left "Expression could not be solved" else
    case expr of
        ElaboratedConstant typ name -> Right $ InferredConstant name $ resolveType typ
        ElaboratedApplication typ left right -> do
            l <- zonking left (residuals, types)
            r <- zonking right (residuals, types)
            Right $ InferredApplication l r $ resolveType typ
        ElaboratedLambda typ (variableName, variableType) nested -> do
            nestedEx <- zonking nested (residuals, types)
            Right $ InferredLambda (variableName, resolveType variableType) nestedEx (resolveType typ)
        ElaboratedTuple typ left right -> do
            leftType <- zonking left (residuals, types)
            rightType <- zonking right (residuals, types)
            Right $ InferredTuple leftType rightType $ resolveType typ
    where
        resolveType :: UnificationType -> InferredType
        resolveType (UnificationVariable var) = case Map.lookup (UnificationVariable var) types of
            Nothing -> error $ "variable " ++ show var ++ " not resolved"
            Just x -> resolveType x
        resolveType (UnificationConstructedType name ts) = InferredConstructedType name $ map resolveType ts
        resolveType (UnificationConstantType name) = InferredConstantType name


