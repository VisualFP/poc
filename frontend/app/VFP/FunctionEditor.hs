module VFP.FunctionEditor where

import Data.Maybe (catMaybes)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import VFP.UI.UIModel ( Identifier, Type (..), TypedValue(..), TypedValue(TypedReference), UntypedValue (Reference, Lambda, TypeHole), UntypedArguments (ArgumentList, ToFill), InferenceResult (Success, Error) )
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
replaceTypeHoleWithTypedValue droppedFunctionName targetTypeHoleId definedValue = do
  runFunction $ ffi $ "console.log('working on function: " ++ show definedValue ++ ", dropped function: " ++ droppedFunctionName ++ ", into type hole: " ++ targetTypeHoleId ++ "')"
  let maybeTypedPreludeValue = getTypedValueFromPrelude droppedFunctionName
  case maybeTypedPreludeValue of
    Just typedPreludeValue -> do
      let untypedValue = insertTypedValueIntoTypeHole typedPreludeValue targetTypeHoleId definedValue
      runFunction $ ffi $ "console.log('toinfer " ++ show untypedValue ++ "')"
      let inferenceResult = infere untypedValue
      runFunction $ ffi $ "console.log('infered " ++ show inferenceResult ++ "')"
      case inferenceResult of
        Success updatedValue -> return $ UpdateSuccess updatedValue
        Error e -> return $ UpdateError e
    Nothing -> do
      runFunction $ ffi $ "console.error('failed to locate prelude function " ++ droppedFunctionName ++ "')"
      return $ UpdateError ("Failed to locate prelude element " ++ droppedFunctionName)

getTypedValueFromPrelude :: String -> Maybe TypedValue
getTypedValueFromPrelude fName = find (isTypedValueFunctionWithName fName) prelude

isTypedValueFunctionWithName :: String -> TypedValue -> Bool
isTypedValueFunctionWithName fName (TypedReference _ refName _) = refName == fName
isTypedValueFunctionWithName _ _ = False

insertTypedValueIntoTypeHole :: TypedValue -> String -> TypedValue -> UntypedValue
insertTypedValueIntoTypeHole valueToInsert targetTypeHoleId (TypedReference refType refName refArgs) = Reference (Just refType) refName (ArgumentList (map (insertTypedValueIntoTypeHole valueToInsert targetTypeHoleId) refArgs))
insertTypedValueIntoTypeHole valueToInsert targetTypeHoleId (TypedLambda _ (_, lambdaParam) lambdaValue) = Lambda lambdaParam (insertTypedValueIntoTypeHole valueToInsert targetTypeHoleId lambdaValue)
insertTypedValueIntoTypeHole valueToInsert targetTypeHoleId (TypedTypeHole typeHoleType typeHoleId) = do
  if typeHoleId == targetTypeHoleId
    then do
      case valueToInsert of
        TypedReference typeToInsert identifiertToInsert _ -> Reference (Just typeToInsert) identifiertToInsert (ToFill typeHoleType)
        _ -> TypeHole
  else TypeHole
