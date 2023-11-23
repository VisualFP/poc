module VFP.UI.UIModel where
import qualified Data.Map.Strict as Map

type Identifier = String
data Type = Primitive String | UnknownType | Function Type Type deriving Eq

instance Show Type where
    show (Primitive name) = name
    show UnknownType = "None"
    show (Function from to) = show from ++ " -> " ++ show to

-- The root value is always a string for now
data ValueDefinition = ValueDefinition { definitionType :: Type, definitionName :: Identifier, definitionValue :: UntypedValue }

data TypedValue = TypedTypeHole Type Identifier -- Identifier = Increasing, inkonsistent number
                | TypedLambda Type (Type, Identifier) TypedValue
                | TypedReference Type Identifier [TypedValue]
                deriving Show

--                      TypeHole      FilledArgs
data UntypedArguments = ToFill Type | ArgumentList [UntypedValue] deriving Show

data UntypedValue = TypeHole
                  | Lambda Identifier UntypedValue
                  | Reference (Maybe Type) Identifier UntypedArguments
                  deriving Show

data InferenceResult = Error String | Success TypedValue deriving Show
