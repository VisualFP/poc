module VFP.UI.Functions where

import VFP.UI.UIModel

functions :: [( String, ValueDefinition )]
functions = [
        ("id1", ValueDefinition { definitionReference = ValueReference { referenceId = 1, referenceName = "Function 1" }, definitionValue = TypeHoleValue 123 } )
        --("id2", ValueDefinition { functionId = "id2", functionName = "Function 2", definition = UserFunction (TypeHoleArg (TypeHole "typeHole2" "Int"))} ),
        --("id3", ValueDefinition { functionId = "id3", functionName = "Function 3", definition = UserFunction (FunctionValue (FunctionCallValue (FunctionCall { functionCallId = "id2", functionArgs = [TypeHoleParam (TypeHole "typeHole1" "String"), TypeHoleParam (TypeHole "typeHole2" "String")] }))) }),
        --("id4", ValueDefinition {
        --  functionId = "id3",
        --  functionName = "Function 3",
        --  definition = UserFunction (
        --      FunctionValue (
        --          FunctionCallValue (
        --              FunctionCall {
        --                  functionCallId = "id2",
        --                  functionArgs = [
        --                      ArgumentValue (
        --                          FunctionRefValue (
        --                              FunctionRef {
        --                                  functionRefId = "id1"
        --                              }
        --                          )
        --                      ),
        --                      TypeHoleParam (TypeHole "typeHole2" "String")
        --                  ]
        --              }
        --          )
        --      )
        --  )
        --})
    ]

lookupFunction :: String -> Maybe ValueDefinition
lookupFunction functionId = lookup functionId functions