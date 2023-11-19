module VFP.FunctionEditor where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import VFP.UI.UIModel
import Data.Maybe (maybeToList, catMaybes)
import VFP.UI.Functions (lookupFunction)

data FunctionDroppedEvent = FunctionDroppedEvent
    { functionDropTargetId :: String
    , functionDragData :: UI.DragData
    } deriving Show

generateComposedFunction :: Function -> UI Element
generateComposedFunction function = do
    functionElement <- UI.new #. "composed-function function-editor-element"
    _ <- element functionElement #+ [UI.p # set UI.text (functionName function)]
    _ <- element functionElement #+ maybeToList (generateFunctionDefinitionElement $ definition function)
    return functionElement


generateFunctionDefinitionElement :: FunctionDefinition -> Maybe (UI Element)
generateFunctionDefinitionElement (UserFunction f) = Just (generateFunctionDefinition f)
generateFunctionDefinitionElement BuiltInFunction = Nothing

generateFunctionDefinition :: UserDefinedFunction -> UI Element
generateFunctionDefinition (TypeHoleArg typeHole) = generateTypeHole typeHole
generateFunctionDefinition (FunctionValue v) = generateFunctionValue v

generateTypeHole :: TypeHole -> UI Element
generateTypeHole TypeHole { typeHoleSignature = typeSignature, typeHoleId = holeId } = UI.new # set UI.text typeSignature
                                                                                                                        # set UI.id_ holeId
                                                                                                                        #. "type-hole"
                                                                                                                        # set UI.droppable True

generateFunctionValue :: Value -> UI Element
generateFunctionValue (LambdaBindingValue l) = UI.new # set UI.text "Not implemented"
generateFunctionValue (FunctionRefValue (FunctionRef { functionRefId = refId })) = do
    let maybeReferencedFunction = lookupFunction refId
    case maybeReferencedFunction of
        Just referencedFunction -> UI.new # set UI.text (functionName referencedFunction)
        Nothing -> do
            runFunction $ ffi $ "console.log('failed to find function with id " ++ refId ++ "')"
            UI.new #. "error"
generateFunctionValue (TypeValueArgument typeValue) = UI.new # set UI.text "Not implemented"
generateFunctionValue (FunctionCallValue (FunctionCall { functionCallId = callId, functionArgs = args })) = do
    let maybeCalledFunction = lookupFunction callId
    case maybeCalledFunction of
        Just calledFunction -> do
            functionCallElement <- UI.new # set UI.text (functionName calledFunction)
                                          #. "function-call function-editor-element"
            let argumentElements = map generateFunctionArguments args
            _ <- element functionCallElement #+ argumentElements
            return functionCallElement
        Nothing -> do
            runFunction $ ffi $ "console.log('failed to find function with id " ++ callId ++ "')"
            UI.new #. "error"

generateFunctionArguments :: FunctionArgument -> UI Element
generateFunctionArguments (TypeHoleParam typeHole) = generateTypeHole typeHole
generateFunctionArguments (ArgumentValue v) = generateFunctionValue v

getTypeHolesFromFunction :: FunctionDefinition -> [TypeHole]
getTypeHolesFromFunction (UserFunction (TypeHoleArg t)) = [t]
getTypeHolesFromFunction (UserFunction (FunctionValue fValue)) = getTypeHolesFromFunctionValue fValue
getTypeHolesFromFunction _ = []

getTypeHolesFromFunctionValue :: Value -> [TypeHole]
getTypeHolesFromFunctionValue (FunctionCallValue (FunctionCall { functionArgs = args })) = concatMap getTypeHolesFromFunctionArguments args
getTypeHolesFromFunctionValue _ = [] -- TODO: get type holes from other values

getTypeHolesFromFunctionArguments :: FunctionArgument -> [TypeHole]
getTypeHolesFromFunctionArguments (TypeHoleParam typeHole) = [typeHole]
getTypeHolesFromFunctionArguments _ = [] -- TODO: get type holes from other function arguments

getFunctionDroppedEvents :: Window -> [TypeHole] -> UI (Maybe [Event FunctionDroppedEvent])
getFunctionDroppedEvents window typeHoles = do
    maybeEvents <- mapM (getFunctionDroppedEvent window) typeHoles
    let events = catMaybes maybeEvents
    if null events
        then return Nothing
        else return $ Just events

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
    runFunction $ ffi $ "console.log('create event for " ++ holeId ++ "')"
    return $ createFunctionDroppedEventFromDropEvent dropEvent holeId


createFunctionDroppedEventFromDropEvent :: Event UI.DragData -> String -> Event FunctionDroppedEvent
createFunctionDroppedEventFromDropEvent dropEvent holeId = fmap (FunctionDroppedEvent holeId) dropEvent

replaceTypeHoleWithFunction :: String -> String -> String -> UI ()
replaceTypeHoleWithFunction composedFunctionId droppedFunctionId targetTypeHoleId = do
    runFunction $ ffi $ "console.log('working on function: " ++ composedFunctionId ++ ", dropped function: " ++ droppedFunctionId ++ ", into type hole: " ++ targetTypeHoleId ++ "')"
    return ()