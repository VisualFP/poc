-- Copyright (C) 2023 Lukas Streckeisen & Jann Flepp
{-# LANGUAGE InstanceSigs #-}

module VFP.UI.UIModel where

import Data.Char
import qualified Data.Set as Set

type Identifier = String
data Type = Primitive String
          | Generic Int
          | List Type
          | Function Type Type deriving Eq

instance Show Type where
    show :: Type -> String
    show typ =
        let generics = getGenerics typ in
        if null generics
        then showType typ
        else "∀ " ++ unwords (map showGeneric $ Set.toList $ Set.fromList $ getGenerics typ) ++ " . " ++ showType typ
      where
        getGenerics :: Type -> [Int]
        getGenerics (Primitive _) = []
        getGenerics (Generic num) = [num]
        getGenerics (List inner) = getGenerics inner
        getGenerics (Function from to) = getGenerics from ++ getGenerics to

        showType :: Type -> String
        showType (Primitive name) = name
        showType (Generic num) = showGeneric num
        showType (List inner) = "[" ++ showType inner ++ "]"
        showType (Function (Function fromFrom fromTo) to) = "(" ++ showType (Function fromFrom fromTo) ++ ")" ++ " → " ++ showType to
        showType (Function from to) = showType from ++ " → " ++ showType to

        showGeneric :: Int -> String
        showGeneric num = [chr (ord 'a' - 1 + num)]

data TypedValue = TypedTypeHole Type Identifier -- Identifier = Increasing, inkonsistent number
                | TypedLambda Type (Type, Identifier) TypedValue
                | TypedReference Type Identifier [TypedValue]
                deriving Show

--                      TypeHole      FilledArgs
data UntypedArguments = ToFill Type | ArgumentList [UntypedValue] deriving Show

data UntypedLambdaValue = ValueToFill | LambdaValue UntypedValue deriving Show

data UntypedValue = TypeHole
                  | Lambda (Maybe Type) Identifier UntypedLambdaValue
                  | Reference (Maybe Type) Identifier UntypedArguments
                  deriving Show

data InferenceResult = Error String | Success TypedValue deriving Show

insertTypedValueIntoTypeHole :: TypedValue -> String -> TypedValue -> UntypedValue
insertTypedValueIntoTypeHole valueToInsert targetTypeHoleId (TypedReference refType refName refArgs) = Reference (Just refType) refName (ArgumentList (map (insertTypedValueIntoTypeHole valueToInsert targetTypeHoleId) refArgs))
insertTypedValueIntoTypeHole valueToInsert targetTypeHoleId (TypedLambda lambdaType (_, lambdaParam) lambdaValue) = Lambda (Just lambdaType) lambdaParam (LambdaValue $ insertTypedValueIntoTypeHole valueToInsert targetTypeHoleId lambdaValue)
insertTypedValueIntoTypeHole valueToInsert targetTypeHoleId (TypedTypeHole typeHoleType typeHoleId) = do
  if typeHoleId == targetTypeHoleId
    then do
      case valueToInsert of
        TypedReference typeToInsert identifiertToInsert _ -> Reference (Just typeToInsert) identifiertToInsert (ToFill typeHoleType)
        TypedLambda _ (_, lambdaParam) _ -> Lambda (Just typeHoleType) lambdaParam ValueToFill
        _ -> TypeHole
    else TypeHole

insertUntypedValueIntoTypeHole :: UntypedValue -> String -> TypedValue -> UntypedValue
insertUntypedValueIntoTypeHole valueToInsert targetTypeHoleId (TypedReference refType refName refArgs) = Reference (Just refType) refName (ArgumentList (map (insertUntypedValueIntoTypeHole valueToInsert targetTypeHoleId) refArgs))
insertUntypedValueIntoTypeHole valueToInsert targetTypeHoleId (TypedLambda lambdaType (_, lambdaParam) lambdaValue) = Lambda (Just lambdaType) lambdaParam (LambdaValue $ insertUntypedValueIntoTypeHole valueToInsert targetTypeHoleId lambdaValue)
insertUntypedValueIntoTypeHole valueToInsert targetTypeHoleId (TypedTypeHole _ typeHoleId) = do
  if typeHoleId == targetTypeHoleId
    then valueToInsert
    else TypeHole