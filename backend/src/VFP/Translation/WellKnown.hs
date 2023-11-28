-- Copyright (C) 2023 Lukas Streckeisen & Jann Flepp

module VFP.Translation.WellKnown(PreludeGroup(..), prelude, string) where

import VFP.UI.UIModel

data PreludeGroup = PreludeGroup { name::String, values :: [TypedValue] }

int :: Type
int = Primitive "int"

string :: Type
string = Primitive "string"

generalGroup :: PreludeGroup
generalGroup = PreludeGroup {
    name = "General",
    values = [
        TypedLambda (Function (Generic 1) (Generic 2)) (Generic 1, "a") (TypedTypeHole (Generic 2) "0"),    -- TODO: solve param name problem
        TypedReference (Function (Generic 1) (Generic 1)) "identity" []
    ]
}

intGroup :: PreludeGroup
intGroup = PreludeGroup {
    name = "Integer",
    values = [
        TypedReference int "1" [],
        TypedReference int "2" [],
        TypedReference (Function int (Function int int)) "plus" [],
        TypedReference (Function int string) "intToString" [] 
    ]
}

stringGroup :: PreludeGroup
stringGroup = PreludeGroup {
    name = "String",
    values = [
        TypedReference string "\"test\"" [],
        TypedReference (Function string (Function string string)) "concat" [],
        TypedReference (Function string (Function (List string) string)) "intercalate" []
    ]
}

listGroup :: PreludeGroup
listGroup = PreludeGroup {
    name = "List",
    values = [
        TypedReference (List $ Generic 1) "nil" [],
        TypedReference (Function (Generic 1) (Function (List (Generic 1)) (List (Generic 1)))) "cons" [],
        TypedReference (Function (Function (Generic 1) (Function (Generic 2) (Generic 2))) (Function (Generic 2) (Function (List $ Generic 1) (Generic 2)))) "fold" [],
        TypedReference (Function (Function (Generic 1) (Generic 2)) (Function (List $ Generic 1) (List $ Generic 2))) "map" []
    ]
}


prelude :: [PreludeGroup]
prelude = [generalGroup, intGroup, stringGroup, listGroup]
