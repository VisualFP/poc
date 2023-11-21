module VFP.UI.UIModel where
import qualified Data.Map.Strict as Map

-- Prelude as a well known list of IDs
type ValueId = Int
type TypeMap = Map.Map ValueId String

-- The root value is always a string for now
data ValueReference = ValueReference { referenceName :: String, referenceId :: ValueId }
data ValueDefinition = ValueDefinition { definitionReference :: ValueReference, definitionValue :: Value }

data Arguments = Unknown | ArgumentList [Value]

data Value = TypeHoleValue ValueId
           | LambdaValue ValueReference Value
           | ReferenceValue ValueReference Arguments

data InferenceResult = Error String | Success ValueDefinition TypeMap

--infere :: ValueDefinition -> InferenceResult