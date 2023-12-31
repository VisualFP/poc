-- Copyright (C) 2023 Lukas Streckeisen & Jann Flepp
module VFP.UI.UIModel where

import Data.Char
import qualified Data.Set as Set

type Identifier = String

data Type
  = Primitive String
  | Generic Int
  | List Type
  | Function Type Type
  deriving (Eq)

printGeneric :: Int -> String
printGeneric num = [chr (ord 'a' - 1 + num)]

printFullType :: Type -> String
printFullType typ =
  let generics = getGenerics typ
   in if null generics
        then printShortType typ
        else "∀ " ++ unwords (map printGeneric $ Set.toList $ Set.fromList $ getGenerics typ) ++ " . " ++ printShortType typ
  where
    getGenerics :: Type -> [Int]
    getGenerics (Primitive _) = []
    getGenerics (Generic num) = [num]
    getGenerics (List inner) = getGenerics inner
    getGenerics (Function from to) = getGenerics from ++ getGenerics to

printShortType :: Type -> String
printShortType (Primitive name) = name
printShortType (Generic num) = printGeneric num
printShortType (List inner) = "[" ++ printShortType inner ++ "]"
printShortType (Function (Function fromFrom fromTo) to) = "(" ++ printShortType (Function fromFrom fromTo) ++ ")" ++ " → " ++ printShortType to
printShortType (Function from to) = printShortType from ++ " → " ++ printShortType to

instance Show Type where
  show = printShortType

data TypedValue = TypedTypeHole Type Identifier
                | TypedLambda Type (Type, Identifier) TypedValue
                | TypedReference Type Identifier [TypedValue]
                | TypedLiteral Type String
                | TypedValueDefinition Type String TypedValue
                deriving (Eq, Show)

--                      TypeHole      FilledArgs
data UntypedArguments = ToFill Type | ArgumentList [UntypedValue] | UnknownArgs deriving (Eq, Show)

data UntypedValue = TypeHole
                  | Lambda (Maybe Type) UntypedValue
                  | Reference (Maybe Type) Identifier UntypedArguments
                  | IntegerLiteral (Maybe String)
                  | BooleanLiteral String
                  | StringLiteral (Maybe String)
                  | ValueDefinition (Maybe Type) String UntypedValue
                  deriving (Eq, Show)

data Value = Untyped UntypedValue | Typed TypedValue

data InferenceResult = Error String | Success TypedValue deriving (Show)

insertUntypedValueIntoTypeHole :: UntypedValue -> String -> TypedValue -> UntypedValue
insertUntypedValueIntoTypeHole valueToInsert targetTypeHoleId (TypedReference refType refName refArgs) =
  Reference (Just refType) refName (ArgumentList (map (insertUntypedValueIntoTypeHole valueToInsert targetTypeHoleId) refArgs))
insertUntypedValueIntoTypeHole _ _ (TypedLiteral (Primitive "Int") refName) =
  IntegerLiteral $ Just refName
insertUntypedValueIntoTypeHole _ _ (TypedLiteral (Primitive "Bool") refName) =
  BooleanLiteral refName
insertUntypedValueIntoTypeHole _ _ (TypedLiteral (Primitive "String") refName) =
  StringLiteral $ Just refName
insertUntypedValueIntoTypeHole _ _ (TypedLiteral typ _) = error $ "Unknown literal type " ++ show typ
insertUntypedValueIntoTypeHole valueToInsert targetTypeHoleId (TypedLambda lambdaType _ lambdaValue) =
  Lambda (Just lambdaType) (insertUntypedValueIntoTypeHole valueToInsert targetTypeHoleId lambdaValue)
insertUntypedValueIntoTypeHole valueToInsert targetTypeHoleId (TypedValueDefinition typ name inner) =
  ValueDefinition (Just typ) name $ insertUntypedValueIntoTypeHole valueToInsert targetTypeHoleId inner
insertUntypedValueIntoTypeHole valueToInsert targetTypeHoleId (TypedTypeHole typeHoleType typeHoleId) = do
  if typeHoleId == targetTypeHoleId
    then case valueToInsert of
      Reference typeToInsert identifiertToInsert _ -> Reference typeToInsert identifiertToInsert (ToFill typeHoleType)
      IntegerLiteral value -> IntegerLiteral value 
      BooleanLiteral value -> BooleanLiteral value
      StringLiteral value -> StringLiteral value 
      Lambda _ _ -> Lambda (Just typeHoleType) TypeHole
      _ -> TypeHole
    else TypeHole
