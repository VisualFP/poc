module Functions where

import Model

functions :: [( String, Function )]
functions = [
        ("id1", Function { functionId = "id1", functionName = "Function 1", functionType = [Primitive "String"], definition = UserFunction (TypeHoleArg (TypeHole "typeHole1" "String"))}),
        ("id2", Function { functionId = "id2", functionName = "Function 2", functionType = [Primitive "Int"], definition = UserFunction (TypeHoleArg (TypeHole "typeHole2" "Int"))})
    ]

lookupFunction :: String -> Maybe Function
lookupFunction functionId = lookup functionId functions

lookupFunctionName :: String -> Maybe String
lookupFunctionName functionId = lookupFunction functionId >>= (Just . functionName)