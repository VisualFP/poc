module VFP.Translation.WellKnown where

import VFP.UI.UIModel

int :: Type
int = Primitive "int"

string :: Type
string = Primitive "string"

identity :: TypedValue
identity = TypedReference (Function (Generic 1) (Generic 1)) "identity" []

fold :: TypedValue
fold = TypedReference (Function (Function (Generic 1) (Function (Generic 2) (Generic 2))) (Function (Generic 2) (Function (List $ Generic 1) (Generic 2)))) "fold" []

one :: TypedValue
one = TypedReference int "1" [] 

testString :: TypedValue
testString = TypedReference string "\"test\"" [] 

two :: TypedValue
two = TypedReference int "2" [] 

plus :: TypedValue
plus = TypedReference (Function int (Function int int)) "plus" [] 

intToString :: TypedValue
intToString = TypedReference (Function int string) "intToString" [] 

nil :: TypedValue
nil = TypedReference (List $ Generic 1) "nil" []

cons :: TypedValue
cons = TypedReference (Function (Generic 1) (Function (List (Generic 1)) (List (Generic 1)))) "cons" []

lambda :: TypedValue
lambda = TypedLambda (Function (Generic 1) (Generic 2)) (Generic 1, "a") (TypedTypeHole (Generic 2) "0")    -- TODO: solve param name problem

prelude :: [TypedValue]
prelude = [identity, fold, one, two, plus, testString, intToString, cons, nil, lambda]
