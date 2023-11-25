module VFP.Translation.WellKnown where

import VFP.UI.UIModel

int :: Type
int = Primitive "int"

string :: Type
string = Primitive "string"

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

prelude :: [TypedValue]
prelude = [one, two, plus, testString, intToString]
