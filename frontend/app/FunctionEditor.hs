module FunctionEditor where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Model
import Data.Maybe (maybeToList)

generateComposedFunction :: Function -> UI Element
generateComposedFunction function = do
    functionElement <- UI.new #. "composed-function"
    _ <- element functionElement #+ [UI.p # set UI.text (functionName function)]
    _ <- element functionElement #+ maybeToList (generateFunctionDefinitionElement $ definition function)
    return functionElement

generateFunctionDefinitionElement :: FunctionDefinition -> Maybe (UI Element)
generateFunctionDefinitionElement (UserFunction f) = Just (generateFunctionDefinition f)
generateFunctionDefinitionElement _ = Nothing

generateFunctionDefinition :: UserDefinedFunction -> UI Element
generateFunctionDefinition (TypeHoleArg (TypeHole { signature = typeSignature })) = UI.new # set UI.text typeSignature
                                                                                           #. "type-hole"
generateFunctionDefinition (FunctionValue v) = generateFunctionValue v

generateFunctionValue :: Value -> UI Element
generateFunctionValue (LambdaBindingValue l) = UI.new # set UI.text "Not implemented"
generateFunctionValue (FunctionRefValue fRef) = UI.new # set UI.text "Not implemented"
generateFunctionValue (TypeValueArgument typeValue) = UI.new # set UI.text "Not implemented"
generateFunctionValue (FunctionCallValue fCall) = UI.new # set UI.text "Not implemented"
