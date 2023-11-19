module VFP.UI.Functions where

import VFP.UI.UIModel

functions :: [( String, Function )]
functions = [
        ("id1", Function { functionId = "id1", functionName = "Function 1", definition = UserFunction (TypeHoleArg (TypeHole "typeHole1" "String"))}),
        ("id2", Function { functionId = "id2", functionName = "Function 2", definition = UserFunction (TypeHoleArg (TypeHole "typeHole2" "Int"))})
    ]

lookupFunction :: String -> Maybe Function
lookupFunction functionId = lookup functionId functions