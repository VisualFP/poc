module VFP.Translation.WellKnown where

import qualified Data.Map as Map
import VFP.Inference.InputModel

inputInt :: InputType
inputInt = InputPrimitive "int"

inputString :: InputType
inputString = InputPrimitive "string"

lookupWellKnownInputConstant :: String -> InputExpression
lookupWellKnownInputConstant "1" = InputConstant inputInt "1" 
lookupWellKnownInputConstant "plus" = InputConstant (InputFunction inputInt (InputFunction inputInt inputInt)) "1" 
lookupWellKnownInputType _ = InputConstant inputInt ""
