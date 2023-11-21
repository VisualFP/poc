module VFP.FunctionEditor where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import VFP.UI.UIModel
import Data.Maybe (catMaybes, fromMaybe)

import qualified Data.Map.Strict as Map

data FunctionDroppedEvent = FunctionDroppedEvent
    { functionDropTargetId :: String
    , functionDragData :: UI.DragData
    } deriving (Show, Eq)

generateValueDefinitionElement :: ValueDefinition -> TypeMap -> UI Element
generateValueDefinitionElement (ValueDefinition { definitionReference = ValueReference { referenceName = valueName, referenceId = valueId }, definitionValue = valueDefinition }) typeMap = do
    valueDefinitionElement <- UI.new #. "value-definition function-editor-element"
    _ <- element valueDefinitionElement #+ [UI.p # set UI.text valueName]
    _ <- element valueDefinitionElement #+ [generateValueElement valueDefinition typeMap]
    return valueDefinitionElement


generateValueElement :: Value -> TypeMap -> UI Element
generateValueElement (TypeHoleValue typeHoleId) typeMap = do
    let typeHoleType = fromMaybe "Unknown" (Map.lookup typeHoleId typeMap)
    UI.new # set UI.text typeHoleType
           # set UI.id_ (show typeHoleId)
           #. "type-hole"
           # set UI.droppable True
generateValueElement (LambdaValue valueRef lambdaValue) typeMap = UI.new # set UI.text "Not yet implemented"
generateValueElement (ReferenceValue valueRef args) typeMap = UI.new # set UI.text "Not yet implemented"

getTypeHolesFromValue :: Value -> [ValueId]
getTypeHolesFromValue (TypeHoleValue typeHoleId) = [typeHoleId]
getTypeHolesFromValue (LambdaValue _ lambdaValue) = getTypeHolesFromValue lambdaValue
getTypeHolesFromValue (ReferenceValue _ args) = getTypeHolesFromArguments args

getTypeHolesFromArguments :: Arguments -> [ValueId]
getTypeHolesFromArguments Unknown = []
getTypeHolesFromArguments (ArgumentList values) = concatMap getTypeHolesFromValue values

getFunctionDroppedEvents :: Window -> [ValueId] -> UI (Maybe [Event FunctionDroppedEvent])
getFunctionDroppedEvents window typeHoles = do
    maybeEvents <- mapM (getFunctionDroppedEvent window) typeHoles
    let events = catMaybes maybeEvents
    if null events
        then return Nothing
        else return $ Just events

getFunctionDroppedEventsV2 :: Window -> [ValueId] -> UI [Event FunctionDroppedEvent]
getFunctionDroppedEventsV2 window typeHoles = do
    maybeEvents <- mapM (getFunctionDroppedEvent window) typeHoles
    return $ catMaybes maybeEvents

getFunctionDroppedEvent :: Window -> ValueId -> UI (Maybe (Event FunctionDroppedEvent))
getFunctionDroppedEvent window typeHole = do
    let holeId = show typeHole
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