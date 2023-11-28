-- Copyright (C) 2023 Lukas Streckeisen & Jann Flepp

module VFP.Translation.WellKnown(PreludeGroup(..), prelude, string, int) where

import VFP.UI.UIModel

data PreludeGroup = PreludeGroup { name::String, values :: [UntypedValue] }

int :: Type
int = Primitive "int"

string :: Type
string = Primitive "string"

generalGroup :: PreludeGroup
generalGroup = PreludeGroup {
    name = "General",
    values = [
        Lambda (Just $ Function (Generic 1) (Generic 2)) (LambdaValue TypeHole),
        Reference (Just $ Function (Generic 1) (Generic 1)) "identity" UnknownArgs
    ]
}

intGroup :: PreludeGroup
intGroup = PreludeGroup {
    name = "Integer",
    values = [
        IntegerLiteral Nothing,
        Reference (Just $ Function int (Function int int)) "plus" UnknownArgs,
        Reference (Just $ Function int string) "intToString" UnknownArgs 
    ]
}

stringGroup :: PreludeGroup
stringGroup = PreludeGroup {
    name = "String",
    values = [
        StringLiteral Nothing,
        Reference (Just $ Function string (Function string string)) "concat" UnknownArgs,
        Reference (Just $ Function string (Function (List string) string)) "intercalate" UnknownArgs
    ]
}

listGroup :: PreludeGroup
listGroup = PreludeGroup {
    name = "List",
    values = [
        Reference (Just $ List $ Generic 1) "nil" UnknownArgs,
        Reference (Just $ Function (Generic 1) (Function (List (Generic 1)) (List (Generic 1)))) "cons" UnknownArgs,
        Reference (Just $ Function (Function (Generic 1) (Function (Generic 2) (Generic 2))) (Function (Generic 2) (Function (List $ Generic 1) (Generic 2)))) "fold" UnknownArgs,
        Reference (Just $ Function (Function (Generic 1) (Generic 2)) (Function (List $ Generic 1) (List $ Generic 2))) "map" UnknownArgs
    ]
}


prelude :: [PreludeGroup]
prelude = [generalGroup, intGroup, stringGroup, listGroup]
