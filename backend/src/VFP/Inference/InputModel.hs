module VFP.Inference.InputModel where

data InputType = InputPrimitive String
               | InputUnknownType
               | InputFunction InputType InputType
               | InputTupleType InputType InputType
               deriving (Show, Eq) 

data InputExpression = InputApplication InputType InputExpression InputExpression 
                     | InputConstant InputType String 
                     | InputTuple InputType InputExpression InputExpression 
                     | InputLambda InputType String InputExpression
                     deriving (Show, Eq) 

getInputType :: InputExpression -> InputType 
getInputType (InputApplication typ _ _) = typ
getInputType (InputConstant typ _) = typ
getInputType (InputTuple typ _ _) = typ
getInputType (InputLambda typ _ _) = typ
