-- Copyright (C) 2023 Lukas Streckeisen & Jann Flepp

module VFP.Translation.WellKnown(PreludeGroup(..), prelude, string, int, bool) where

import VFP.UI.UIModel

data PreludeGroup = PreludeGroup { name::String, values :: [UntypedValue] }

int :: Type
int = Primitive "Int"

string :: Type
string = Primitive "String"

bool :: Type
bool = Primitive "Bool"

generalGroup :: PreludeGroup
generalGroup = PreludeGroup {
    name = "General",
    values = [
        Lambda (Just $ Function (Generic 1) (Generic 2)) TypeHole,
        Reference (Just $ Function (Generic 1) (Generic 1)) "identity" UnknownArgs
    ]
}

boolGroup :: PreludeGroup
boolGroup = PreludeGroup {
    name = "Boolean",
    values = [
        BooleanLiteral "True",
        BooleanLiteral "False",
        Reference (Just $ Function bool (Function (Generic 1) (Function (Generic 1) (Generic 1)))) "if" UnknownArgs,
        Reference (Just $ Function bool (Function bool bool)) "and" UnknownArgs,
        Reference (Just $ Function bool (Function bool bool)) "or" UnknownArgs,
        Reference (Just $ Function bool string) "boolToString" UnknownArgs 
    ]
}

intGroup :: PreludeGroup
intGroup = PreludeGroup {
    name = "Integer",
    values = [
        IntegerLiteral Nothing,
        Reference (Just $ Function int (Function int int)) "plus" UnknownArgs,
        Reference (Just $ Function int (Function int bool)) "equals" UnknownArgs,
        Reference (Just $ Function int (Function int bool)) "largerThan" UnknownArgs,
        Reference (Just $ Function int (Function int bool)) "lessThan" UnknownArgs,
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
        Reference (Just $ Function (Function (Generic 1) (Generic 2)) (Function (List $ Generic 1) (List $ Generic 2))) "map" UnknownArgs,
        Reference (Just $ Function (Function (Generic 1) bool) (Function (List $ Generic 1) (List $ Generic 1))) "filter" UnknownArgs
    ]
}

prelude :: [PreludeGroup]
prelude = [generalGroup, boolGroup, intGroup, stringGroup, listGroup]
