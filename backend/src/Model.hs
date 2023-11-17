module Model where

data Function = Function
    { functionId :: String
    , functionName :: String
    , functionType :: FunctionType
    , definition :: FunctionDefinition
    }

data FunctionDefinition = SystemFunction BuiltInFunction | UserFunction UserDefinedFunction

type BuiltInFunction = String

data Type = FuncType FunctionType
            | Primitive PrimitiveType
            | Sum SumType

type FunctionType = [Type]

type PrimitiveType = String

data SumType = SumType
    { typeName :: String
    , constructors :: [TypeConstructor]
    }

data TypeConstructor = TypeConstructor
    { label :: String
    , constructorArgs :: [Type]
    }

data TypeHole = TypeHole
  { signature :: String
  } deriving (Show)

data UserDefinedFunction = FunctionValue Value | TypeHoleArg TypeHole
  deriving (Show)

data PrimitiveValue = IntValue Int | DoubleValue Double | CharValue Char | StringValue String
  deriving (Show)

data TypeArgument = TypeValueArg TypeValue | PrimitiveTypeArg PrimitiveValue
  deriving (Show)

data FunctionArgument = LambdaArg LambdaBinding | FunctionRefArg FunctionRef | TypeArg TypeArgument | TypeHoleParam TypeHole
  deriving (Show)

data FunctionCall = FunctionCall
  { functionCallId :: String,
    functionArgs :: [FunctionArgument]
  }
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

data TypeValue = TypeValue
  { valueType :: String,
    typeArgs :: [TypeArgument]
  }
  deriving (Show)

data Value
  = LambdaBindingValue LambdaBinding
  | FunctionRefValue FunctionRef
  | TypeValueArgument TypeValue
  | FunctionCallValue FunctionCall
  deriving (Show)
