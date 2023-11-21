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

--                      TypeHole      FilledArgs
data UntypedArguments = ToFill Type | ArgumentList [UntypedValue]

data UntypedValue = TypeHole
                  | Lambda Identifier UntypedValue
                  | Reference (Maybe Type) Identifier UntypedArguments

data InferenceResult = Error String | Success TypedValue
