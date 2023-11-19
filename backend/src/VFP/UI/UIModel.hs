module VFP.UI.UIModel where

data Function = Function
  { functionId :: String,
    functionName :: String,
    definition :: FunctionDefinition
  }

data FunctionDefinition
  = UserFunction UserDefinedFunction
  | BuiltInFunction

data TypeHole = TypeHole
  { typeHoleId :: String,
    typeHoleSignature :: String
  }
  deriving (Show)

data UserDefinedFunction = FunctionValue Value | TypeHoleArg TypeHole
  deriving (Show)

data Value
  = LambdaBindingValue LambdaBinding
  | FunctionRefValue FunctionRef
  | TypeValueArgument TypeConstructorCall
  | FunctionCallValue FunctionCall
  deriving (Show)

data TypeArgument = TypeValueArg TypeConstructorCall | PrimitiveTypeArg PrimitiveValue
  deriving (Show)

data PrimitiveValue = IntValue Int | DoubleValue Double | CharValue Char | StringValue String
  deriving (Show)

data TypeConstructorCall = TypeValue
  { valueType :: String,
    typeArgs :: [TypeArgument]
  }
  deriving (Show)

data FunctionCall = FunctionCall
  { functionCallId :: String,
    functionArgs :: [FunctionArgument]
  }
  deriving (Show)

data FunctionArgument = ArgumentValue Value | TypeHoleParam TypeHole
  deriving (Show)

data FunctionRef = FunctionRef
  { functionRefId :: String
  }
  deriving (Show)

data LambdaBinding = LambdaBinding
  { param :: String,
    body :: UserDefinedFunction
  }
  deriving (Show)
