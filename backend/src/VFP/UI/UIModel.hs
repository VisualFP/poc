-- Copyright (C) 2023 Lukas Streckeisen & Jann Flepp
module VFP.UI.UIModel where

import Data.Char
import qualified Data.Set as Set
import Data.Either (partitionEithers)

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

data ValueUnderConstruction = ValueUnderConstruction {
  valueName :: String,
  valueDefinition :: TypedValue,
  valueType :: Type
} deriving Show

data UntypedValueUnderConstruction = UntypedValueUnderConstruction Type UntypedValue deriving (Show)

data TypedValue
  = TypedTypeHole Type Identifier -- Identifier = Increasing, inkonsistent number
  | TypedLambda Type (Type, Identifier) TypedValue
  | TypedReference Type Identifier [TypedValue]
  | TypedLiteral Type String
  deriving (Show, Eq)

--                      TypeHole      FilledArgs
data UntypedArguments = ToFill Type | ArgumentList [UntypedValue] | UnknownArgs deriving (Show, Eq)

data UntypedLambdaValue = ValueToFill | LambdaValue UntypedValue deriving (Show, Eq)

data UntypedValue
  = TypeHole
  | Lambda (Maybe Type) UntypedLambdaValue
  | Reference (Maybe Type) Identifier UntypedArguments
  | IntegerLiteral (Maybe String)
  | StringLiteral (Maybe String)
  deriving (Show, Eq)

data Value = Untyped UntypedValue | Typed TypedValue

data InferenceResult = Error String | Success TypedValue deriving (Show)

insertUntypedValueIntoTypeHole :: UntypedValue -> String -> TypedValue -> Either String UntypedValue
insertUntypedValueIntoTypeHole _ _ (TypedLiteral (Primitive "int") refName) = Right $ IntegerLiteral $ Just refName
insertUntypedValueIntoTypeHole _ _ (TypedLiteral (Primitive "string") refName) = Right $ StringLiteral $ Just refName
insertUntypedValueIntoTypeHole _ _ (TypedLiteral typ _) = Left $ "Unknown literal type " ++ show typ
insertUntypedValueIntoTypeHole valueToInsert targetTypeHoleId (TypedReference refType refName refArgs) =
  let (errors, args) = partitionEithers $ map (insertUntypedValueIntoTypeHole valueToInsert targetTypeHoleId) refArgs in
    case errors of
      [] -> Right $ Reference (Just refType) refName (ArgumentList args)
      (e:_) -> Left e
insertUntypedValueIntoTypeHole valueToInsert targetTypeHoleId (TypedLambda lambdaType _ lambdaValue) =
  case insertUntypedValueIntoTypeHole valueToInsert targetTypeHoleId lambdaValue of
    Right lambdaBody -> Right $ Lambda (Just lambdaType) (LambdaValue lambdaBody)
    Left e -> Left e
insertUntypedValueIntoTypeHole valueToInsert targetTypeHoleId (TypedTypeHole typeHoleType typeHoleId) =
  if typeHoleId == targetTypeHoleId
    then case valueToInsert of
      Reference typeToInsert identifiertToInsert _ -> Right $ Reference typeToInsert identifiertToInsert (ToFill typeHoleType)
      IntegerLiteral value -> Right $ IntegerLiteral value
      StringLiteral value -> Right $ StringLiteral value
      Lambda _ _ -> Right $ Lambda (Just typeHoleType) ValueToFill
      TypeHole -> Right TypeHole
    else Right TypeHole
