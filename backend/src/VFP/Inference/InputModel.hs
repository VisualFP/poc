-- Copyright (C) 2023 Lukas Streckeisen & Jann Flepp

module VFP.Inference.InputModel where

data InputType = InputPrimitive String
               | InputUnknownType
               | InputFunction InputType InputType
               | InputTupleType InputType InputType
               | InputList InputType
               | InputGeneric Int -- Counter inside type to re-use
               deriving (Show, Eq) 

data InputExpression = InputApplication InputType InputExpression InputExpression 
                     | InputConstant InputType String 
                     | InputTuple InputType InputExpression InputExpression 
                     | InputLambda InputType String InputExpression
                     | InputValueDefinition InputType InputExpression
                     | InputTypeHole InputType
                     deriving (Show, Eq) 

getInputType :: InputExpression -> InputType 
getInputType (InputValueDefinition typ _) = typ
getInputType (InputApplication typ _ _) = typ
getInputType (InputConstant typ _) = typ
getInputType (InputTuple typ _ _) = typ
getInputType (InputLambda typ _ _) = typ
getInputType (InputTypeHole typ) = typ
