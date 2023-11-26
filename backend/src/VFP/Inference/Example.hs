{-# OPTIONS_GHC -Wno-missing-signatures #-}
module VFP.Inference.Example where

import VFP.Inference.Unification
import VFP.Inference.InputModel
import VFP.Inference.Zonking


inputInt = InputPrimitive "int"
inputString = InputPrimitive "string"
inputA = InputConstant inputInt "a" 
inputB = InputConstant inputInt "b" 
inputPlus = InputConstant (InputFunction inputInt (InputFunction inputInt inputInt)) "plus" 
inputUnknownType = InputUnknownType
inputHole1 = InputConstant inputUnknownType "?1" 
inputHole2 = InputConstant inputUnknownType "?2" 

unifAlpha = UnificationVariable "α" False
unifBeta = UnificationVariable "β" False
unifGamma = UnificationVariable "γ" False
unifDelta = UnificationVariable "δ" False
unifInt = UnificationConstantType "Int"
unifBool = UnificationConstantType "Bool"
unifString = UnificationConstantType "String"
unifList x = UnificationConstructedType "list" [x]
unifFunction from to = UnificationConstructedType "->" [from, to]

inferedInt = InferedConstantType "int"

