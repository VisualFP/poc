module VFP.UI.Functions where

import VFP.UI.UIModel

functions :: [( String, ValueDefinition )]
functions = [
        ("id1", ValueDefinition { definitionType = "String", definitionName = "functionOne", definitionValue = TypeHole "123" } ),
        ("id2", ValueDefinition { definitionType = "String -> Int -> String", definitionName = "functionTwo", definitionValue = TypeHole "124" } ),
        ("id3", ValueDefinition { definitionType = "String", definitionName = "functionThree", definitionValue = Reference "funcitonTwo" (ArgumentList [TypeHole "125", TypeHole "126"]) } ),
        ("id4", ValueDefinition { definitionType = "String -> Int -> String", definitionName = "functionFour", definitionValue = Lambda "s" (Reference "functionTwo" (ArgumentList [Reference "s" (ArgumentList []), TypeHole "127"])) } )
    ]

typedValues :: [( String, TypedValue)]
typedValues = [
    ("id4", TypedLambda "String -> Int -> String" ("String", "s") (TypedReference "String -> Int -> String" "functionTwo" [TypedReference "String" "s" [], TypedTypeHole "Int" "127"]))
    ]

lookupFunction :: String -> Maybe ValueDefinition
lookupFunction functionId = lookup functionId functions