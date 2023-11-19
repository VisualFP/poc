module FunctionEditor where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import UIModel
import Data.Maybe (maybeToList, catMaybes)

data FunctionDroppedEvent = FunctionDroppedEvent
    { functionDropTargetId :: String
    , functionDragData :: UI.DragData
    } deriving Show

generateComposedFunction :: Function -> UI Element
generateComposedFunction function = do
    functionElement <- UI.new #. "composed-function"
    _ <- element functionElement #+ [UI.p # set UI.text (functionName function)]
    _ <- element functionElement #+ maybeToList (generateFunctionDefinitionElement $ definition function)
    return functionElement


generateFunctionDefinitionElement :: FunctionDefinition -> Maybe (UI Element)
generateFunctionDefinitionElement (UserFunction f) = Just (generateFunctionDefinition f)
generateFunctionDefinitionElement BuiltInFunction = Nothing

generateFunctionDefinition :: UserDefinedFunction -> UI Element
generateFunctionDefinition (TypeHoleArg (TypeHole { typeHoleSignature = typeSignature, typeHoleId = holeId })) = UI.new # set UI.text typeSignature
                                                                                                                        # set UI.id_ holeId
                                                                                                                        #. "type-hole"
                                                                                                                        # set UI.droppable True
generateFunctionDefinition (FunctionValue v) = generateFunctionValue v

generateFunctionValue :: Value -> UI Element
generateFunctionValue (LambdaBindingValue l) = UI.new # set UI.text "Not implemented"
generateFunctionValue (FunctionRefValue fRef) = UI.new # set UI.text "Not implemented"
generateFunctionValue (TypeValueArgument typeValue) = UI.new # set UI.text "Not implemented"
generateFunctionValue (FunctionCallValue fCall) = UI.new # set UI.text "Not implemented"

getTypeHolesFromFunction :: FunctionDefinition -> [TypeHole]
getTypeHolesFromFunction (UserFunction (TypeHoleArg t)) = [t]
getTypeHolesFromFunction (UserFunction (FunctionValue fv)) = [] -- TODO: get type holes from function value
getTypeHolesFromFunction _ = []

getFunctionDroppedEvents :: Window -> [TypeHole] -> UI (Maybe (Event [FunctionDroppedEvent]))
getFunctionDroppedEvents window typeHoles = do
    maybeEvents <- mapM (getFunctionDroppedEvent window) typeHoles
    let events = catMaybes maybeEvents
    if null events
        then return Nothing
        else return $ Just $ unions events

getFunctionDroppedEvent :: Window -> TypeHole -> UI (Maybe (Event FunctionDroppedEvent))
getFunctionDroppedEvent window typeHole = do
    let holeId = typeHoleId typeHole
    maybeTypeHoleElement <- getElementById window holeId
    case maybeTypeHoleElement of
        Just typeHoleElement -> do
            event <- createFunctionDroppedEvent typeHoleElement holeId
            return $ Just event
        Nothing -> return Nothing


createFunctionDroppedEvent :: Element -> String -> UI (Event FunctionDroppedEvent)
createFunctionDroppedEvent typeHoleElement holeId = do
    let dropEvent = UI.drop typeHoleElement
    -- on UI.drop typeHoleElement $ \_ -> runFunction $ ffi "console.log('HI')"
    runFunction $ ffi $ "console.log('create event for " ++ holeId ++ "')"
    return $ createFunctionDroppedEventFromDropEvent dropEvent holeId


createFunctionDroppedEventFromDropEvent :: Event UI.DragData -> String -> Event FunctionDroppedEvent
createFunctionDroppedEventFromDropEvent dropEvent holeId = fmap (FunctionDroppedEvent holeId) dropEvent

replaceTypeHoleWithFunction :: String -> String -> String -> UI ()
replaceTypeHoleWithFunction composedFunctionId droppedFunctionId targetTypeHoleId = do
    runFunction $ ffi $ "console.log('working on function: " ++ composedFunctionId ++ ", dropped function: " ++ droppedFunctionId ++ ", into type hole: " ++ targetTypeHoleId ++ "')"
    return ()