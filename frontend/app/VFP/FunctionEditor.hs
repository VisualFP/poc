module VFP.FunctionEditor where

import Data.Maybe (catMaybes)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import VFP.UI.UIModel
import Data.Foldable (find)
import VFP.Translation.WellKnown (prelude)
import VFP.Translation.InferenceTranslation (infere)

data FunctionDroppedEvent = FunctionDroppedEvent
  { functionDropTargetId :: String,
    functionDragData :: UI.DragData
  }
  deriving (Show, Eq)

data ValueDefinitionUpdateResult = UpdateSuccess TypedValue
                                 | UpdateError String

generateValueDefinitionElement :: Identifier -> Type -> TypedValue -> UI Element
generateValueDefinitionElement defName defType valueDefinition = do
  valueDefinitionElement <- UI.new #. "value-definition function-editor-element"
  _ <- element valueDefinitionElement #+ [UI.p # set UI.text defName]
  _ <- element valueDefinitionElement #+ [generateValueElement valueDefinition]
  definitionTypeElement <- UI.p # set UI.text (show defType)
                                #. "definition-type"
  _ <- element valueDefinitionElement #+ [element definitionTypeElement]
  return valueDefinitionElement

generateValueElement :: TypedValue -> UI Element
generateValueElement (TypedTypeHole holeType holeId) = do
  UI.new
    # set UI.text (show holeType)
    # set UI.id_ holeId
    #. "type-hole function-editor-element"
    # set UI.droppable True
    # set (UI.attr "title") "" -- to prevent tooltips from wrapper elements to appear
generateValueElement (TypedLambda lambdaType (paramType, paramName) lambdaValue) = do
  lambdaElement <-
    UI.new
      #. "lambda function-editor-element"
      # set (UI.attr "title") (show lambdaType)
  lambdaIcon <-
    UI.p
      # set UI.text "λ"
      #. "lambda-icon"
  lambdaParameter <-
    UI.new
      # set UI.text paramName
      #. "lambda-parameter"
      # set (UI.attr "title") (show paramType)
  lambdaValueElement <- generateValueElement lambdaValue
  _ <- element lambdaElement #+ [element lambdaIcon, element lambdaParameter, element lambdaValueElement]
  return lambdaElement
generateValueElement (TypedReference refType refName args) = do
  referenceValueElement <-
    UI.new
      #. "reference-value function-editor-element"
      # set (UI.attr "title") (show refType)
  _ <- element referenceValueElement #+ [UI.p # set UI.text refName]
  _ <- element referenceValueElement #+ map generateValueElement args
  return referenceValueElement

getTypeHolesFromValue :: TypedValue -> [String]
getTypeHolesFromValue (TypedTypeHole _ typeHoleId) = [typeHoleId]
getTypeHolesFromValue (TypedLambda _ _ lambdaValue) = getTypeHolesFromValue lambdaValue
getTypeHolesFromValue (TypedReference _ _ args) = concatMap getTypeHolesFromValue args

getFunctionDroppedEvents :: Window -> [String] -> UI [Event FunctionDroppedEvent]
getFunctionDroppedEvents window typeHoles = do
  maybeEvents <- mapM (getFunctionDroppedEvent window) typeHoles
  return $ catMaybes maybeEvents

getFunctionDroppedEventsV2 :: Window -> [String] -> UI [Event FunctionDroppedEvent]
getFunctionDroppedEventsV2 window typeHoles = do
  maybeEvents <- mapM (getFunctionDroppedEvent window) typeHoles
  return $ catMaybes maybeEvents

getFunctionDroppedEvent :: Window -> String -> UI (Maybe (Event FunctionDroppedEvent))
getFunctionDroppedEvent window typeHoleId = do
  maybeTypeHoleElement <- getElementById window typeHoleId
  case maybeTypeHoleElement of
    Just typeHoleElement -> do
      event <- createFunctionDroppedEvent typeHoleElement typeHoleId
      return $ Just event
    Nothing -> do
        runFunction $ ffi $ "console.error('Failed to find type hole element with id " ++ typeHoleId ++ ")"
        return Nothing

createFunctionDroppedEvent :: Element -> String -> UI (Event FunctionDroppedEvent)
createFunctionDroppedEvent typeHoleElement holeId = do
  let dropEvent = UI.drop typeHoleElement
  runFunction $ ffi $ "console.log('create event for " ++ holeId ++ "')"
  return $ createFunctionDroppedEventFromDropEvent dropEvent holeId

createFunctionDroppedEventFromDropEvent :: Event UI.DragData -> String -> Event FunctionDroppedEvent
createFunctionDroppedEventFromDropEvent dropEvent holeId = fmap (FunctionDroppedEvent holeId) dropEvent

replaceTypeHoleWithTypedValue:: String -> String -> TypedValue -> UI ValueDefinitionUpdateResult
replaceTypeHoleWithTypedValue typedValueNameToInsert targetTypeHoleId definedValue = do
  runFunction $ ffi $ "console.log('working on function: " ++ show definedValue ++ ", dropped value: " ++ typedValueNameToInsert ++ ", into type hole: " ++ targetTypeHoleId ++ "')"
  let maybeTypedPreludeValue = getTypedValueFromPrelude typedValueNameToInsert
  case maybeTypedPreludeValue of
    Just typedPreludeValue -> do
      case insertTypedValueIntoTypeHole typedPreludeValue targetTypeHoleId definedValue of
        Right untypedValue -> do
          runFunction $ ffi $ "console.log('toinfer " ++ show untypedValue ++ "')"
          let inferenceResult = infere untypedValue
          runFunction $ ffi $ "console.log('infered " ++ show inferenceResult ++ "')"
          case inferenceResult of
            Success updatedValue -> return $ UpdateSuccess updatedValue
            Error e -> return $ UpdateError e
        Left e -> return $ UpdateError e
    Nothing -> do
      runFunction $ ffi $ "console.error('failed to locate prelude function " ++ typedValueNameToInsert ++ "')"
      return $ UpdateError ("Failed to locate prelude element " ++ typedValueNameToInsert)

getTypedValueFromPrelude :: String -> Maybe TypedValue
getTypedValueFromPrelude name = find (isTypedValueWithName name) prelude

isTypedValueWithName :: String -> TypedValue -> Bool
isTypedValueWithName name (TypedReference _ refName _) = refName == name
isTypedValueWithName name (TypedLambda {}) = name == "lambda"
isTypedValueWithName _ _ = False

insertTypedValueIntoTypeHole :: TypedValue -> String -> TypedValue -> Either String UntypedValue
insertTypedValueIntoTypeHole valueToInsert targetTypeHoleId (TypedReference refType refName refArgs) = do
  case mapM (insertTypedValueIntoTypeHole valueToInsert targetTypeHoleId) refArgs of
    Right arguments -> Right $ Reference (Just refType) refName (ArgumentList arguments)
    Left e -> Left e
insertTypedValueIntoTypeHole valueToInsert targetTypeHoleId (TypedLambda lambdaType (_, lambdaParam) lambdaValue) = do
  case insertTypedValueIntoTypeHole valueToInsert targetTypeHoleId lambdaValue of
    Right lambaBody -> Right $ Lambda (Just lambdaType) lambdaParam (LambdaValue lambaBody)
    Left e -> Left e
insertTypedValueIntoTypeHole valueToInsert targetTypeHoleId (TypedTypeHole typeHoleType typeHoleId) = do
  if typeHoleId == targetTypeHoleId
    then do
      case valueToInsert of
        TypedReference typeToInsert identifiertToInsert _ -> Right $ Reference (Just typeToInsert) identifiertToInsert (ToFill typeHoleType)
        TypedLambda _ (_, lambdaParam) _ -> Right $ Lambda (Just typeHoleType) lambdaParam ValueToFill
        _ -> Right TypeHole
  else Right TypeHole