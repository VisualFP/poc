module VFP.UI.UIModel where
import qualified Data.Map.Strict as Map

type Identifier = String
type Type = String

-- The root value is always a string for now
data ValueDefinition = ValueDefinition { definitionType :: Type, definitionName :: Identifier, definitionValue :: UntypedValue }

data TypedValue = TypedTypeHole Type Identifier
                | TypedLambda Type (Type, Identifier) TypedValue
                | TypedReference Type Identifier [TypedValue]

data UntypedArguments = Unknown | ArgumentList [UntypedValue]

data UntypedValue = TypeHole Identifier
                  | Lambda Identifier UntypedValue
                  | Reference Identifier UntypedArguments

data InferenceResult = Error String | Success TypedValue

--infere :: UntypedValue -> InferenceResult