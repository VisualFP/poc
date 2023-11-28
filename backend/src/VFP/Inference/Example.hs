-- Copyright (C) 2023 Lukas Streckeisen & Jann Flepp


{-# OPTIONS_GHC -Wno-missing-signatures #-}
module VFP.Inference.Example where

import VFP.Inference.Unification
import VFP.Inference.InputModel
import VFP.Inference.Zonking


inputInt = InputPrimitive "int"
inputString = InputPrimitive "string"
inputA = InputReference inputInt "a" 
inputB = InputReference inputInt "b" 
inputPlus = InputReference (InputFunction inputInt (InputFunction inputInt inputInt)) "plus" 
inputUnknownType = InputUnknownType
inputHole1 = InputReference inputUnknownType "?1" 
inputHole2 = InputReference inputUnknownType "?2" 

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

