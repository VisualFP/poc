module VFP.FunctionEditor where

import Data.Maybe (catMaybes)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import VFP.UI.UIModel

data FunctionDroppedEvent = FunctionDroppedEvent
  { functionDropTargetId :: String,
    functionDragData :: UI.DragData
  }
  deriving (Show, Eq)

generateValueDefinitionElement :: Identifier -> Type -> TypedValue -> UI Element
generateValueDefinitionElement defName defType valueDefinition = do
  valueDefinitionElement <-
    UI.new
      #. "value-definition function-editor-element"
      # set (UI.attr "title") defType
  _ <- element valueDefinitionElement #+ [UI.p # set UI.text defName]
  _ <- element valueDefinitionElement #+ [generateValueElement valueDefinition]
  return valueDefinitionElement

generateValueElement :: TypedValue -> UI Element
generateValueElement (TypedTypeHole holeType holeId) = do
  UI.new
    # set UI.text holeType
    # set UI.id_ holeId
    #. "type-hole function-editor-element"
    # set UI.droppable True
    # set (UI.attr "title") "" -- to prevent tooltips from wrapper elements to appear
generateValueElement (TypedLambda lambdaType (paramType, paramName) lambdaValue) = do
  lambdaElement <-
    UI.new
      #. "lambda function-editor-element"
      # set (UI.attr "title") lambdaType
  lambdaIcon <-
    UI.p
      # set UI.text "λ"
      #. "lambda-icon"
  lambdaParameter <-
    UI.new
      # set UI.text paramName
      #. "lambda-parameter"
      # set (UI.attr "title") paramType
  lambdaValueElement <- generateValueElement lambdaValue
  _ <- element lambdaElement #+ [element lambdaIcon, element lambdaParameter, element lambdaValueElement]
  return lambdaElement
generateValueElement (TypedReference refType refName args) = do
  referenceValueElement <-
    UI.new
      #. "reference-value function-editor-element"
      # set (UI.attr "title") refType
  _ <- element referenceValueElement #+ [UI.p # set UI.text refName]
  _ <- element referenceValueElement #+ map generateValueElement args
  return referenceValueElement

getTypeHolesFromValue :: TypedValue -> [String]
getTypeHolesFromValue (TypedTypeHole _ typeHoleId) = [typeHoleId]
getTypeHolesFromValue (TypedLambda _ _ lambdaValue) = getTypeHolesFromValue lambdaValue
getTypeHolesFromValue (TypedReference _ _ args) = concatMap getTypeHolesFromValue args

getFunctionDroppedEvents :: Window -> [String] -> UI (Maybe [Event FunctionDroppedEvent])
getFunctionDroppedEvents window typeHoles = do
  maybeEvents <- mapM (getFunctionDroppedEvent window) typeHoles
  let events = catMaybes maybeEvents
  if null events
    then return Nothing
    else return $ Just events

getFunctionDroppedEventsV2 :: Window -> [String] -> UI [Event FunctionDroppedEvent]
getFunctionDroppedEventsV2 window typeHoles = do
  maybeEvents <- mapM (getFunctionDroppedEvent window) typeHoles
  return $ catMaybes maybeEvents

getFunctionDroppedEvent :: Window -> String -> UI (Maybe (Event FunctionDroppedEvent))
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