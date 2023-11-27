module VFP.UI.UIModel where

import Data.Char

type Identifier = String
data Type = Primitive String
          | Generic Int
          | List Type
          | Function Type Type deriving Eq

instance Show Type where
    show (Primitive name) = name
    show (Generic num) = [chr (ord 'a' - 1 + num)]
    show (List inner) = "[" ++ show inner ++ "]"
    show (Function (Function fromFrom fromTo) to) = "(" ++ show (Function fromFrom fromTo) ++ ")" ++ " → " ++ show to
    show (Function from to) = show from ++ " → " ++ show to

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
