module Functions where

import UIModel

functions :: [( String, Function )]
functions = [
        ("id1", Function { functionId = "id1", functionName = "Function 1", definition = UserFunction (TypeHoleArg (TypeHole "typeHole1" "String"))} ),
        ("id2", Function { functionId = "id2", functionName = "Function 2", definition = UserFunction (TypeHoleArg (TypeHole "typeHole2" "Int"))} ),
        ("id3", Function { functionId = "id3", functionName = "Function 3", definition = UserFunction (FunctionValue (FunctionCallValue (FunctionCall { functionCallId = "id2", functionArgs = [TypeHoleParam (TypeHole "typeHole1" "String"), TypeHoleParam (TypeHole "typeHole2" "String")] }))) }),
        ("id4", Function {
            functionId = "id3",
            functionName = "Function 3",
            definition = UserFunction (
                FunctionValue (
                    FunctionCallValue (
                        FunctionCall {
                            functionCallId = "id2",
                            functionArgs = [
                                ArgumentValue (
                                    FunctionRefValue (
                                        FunctionRef {
                                            functionRefId = "id1"
                                        }
                                    )
                                ),
                                TypeHoleParam (TypeHole "typeHole2" "String")
                            ]
                        }
                    )
                )
            )
        })
    ]

lookupFunction :: String -> Maybe Function
lookupFunction functionId = lookup functionId functions