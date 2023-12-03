-- Copyright (C) 2023 Lukas Streckeisen & Jann Flepp

module VFP.FunctionEditor where

import Data.Foldable (find)
import Data.Maybe (catMaybes, mapMaybe, fromMaybe)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import VFP.Translation.InferenceTranslation (infere)
import VFP.Translation.WellKnown (prelude)
import VFP.UI.UIModel
import Data.List (isPrefixOf, stripPrefix)
import qualified VFP.Translation.WellKnown as WellKnown
import Control.Monad (replicateM)
import System.Random

import Debug.Trace

data FunctionDroppedEvent = FunctionDroppedEvent
  { functionDropTargetId :: String,
    functionDragData :: UI.DragData
  }
  deriving (Show, Eq)

data ValueDefinitionUpdateResult
  = UpdateSuccess TypedValue
  | UpdateError String

generateValueElement :: TypedValue -> UI Element
generateValueElement (TypedValueDefinition typ name inner) = do
  valueDefinitionElement <- UI.new #. "value-definition function-editor-element"
  definitionReference <-
    UI.new
      #+ [UI.p # set UI.text name]
      #. "definition-reference"
      # set (UI.attr "title") (printFullType typ)
      # set UI.draggable True
      # set UI.dragData ("definitionReference-" ++ name)
  _ <- element valueDefinitionElement #+ [element definitionReference]
  _ <- element valueDefinitionElement #+ [generateValueElement inner]
  definitionTypeElement <-
    UI.p
      # set UI.text (printShortType typ)
      #. "definition-type"
  _ <- element valueDefinitionElement #+ [element definitionTypeElement]
  return valueDefinitionElement
generateValueElement (TypedTypeHole holeType holeId) = do
  UI.new
    # set UI.id_ holeId
    #. "type-hole function-editor-element"
    # set UI.droppable True
    # set (UI.attr "title") "" -- to prevent tooltips from wrapper elements to appear
    #+ [UI.p # set UI.text (printShortType holeType)]
generateValueElement (TypedLambda lambdaType (paramType, paramName) lambdaValue) = do
  lambdaElement <-
    UI.new
      #. "lambda function-editor-element"
      # set (UI.attr "title") (printFullType lambdaType)
  lambdaIcon <-
    UI.span
      # set UI.text "λ"
      #. "lambda-icon"
  lambdaParameter <-
    UI.new
      #+ [UI.p # set UI.text paramName]
      #. "lambda-parameter"
      # set (UI.attr "title") (printFullType paramType)
      # set UI.draggable True
      # set UI.dragData ("lambdaParam-" ++ paramName)
  lambdaValueElement <- generateValueElement lambdaValue
  _ <- element lambdaElement #+ [element lambdaIcon, element lambdaParameter, element lambdaValueElement]
  return lambdaElement
generateValueElement (TypedReference refType refName args) = do
  referenceValueElement <-
    UI.new
      #. "reference-value function-editor-element"
      # set (UI.attr "title") (printFullType refType)
  _ <- element referenceValueElement #+ [UI.p # set UI.text refName]
  _ <- element referenceValueElement #+ map generateValueElement args
  return referenceValueElement
generateValueElement (TypedLiteral refType refName) = do
  literalValueElement <-
    UI.new
      #. "literal-value literal function-editor-element"
      # set (UI.attr "title") (printFullType refType)
  _ <- element literalValueElement #+ [UI.p # set UI.text refName]
  return literalValueElement

getTypeHolesFromValue :: TypedValue -> [String]
getTypeHolesFromValue (TypedValueDefinition _ _ inner) = getTypeHolesFromValue inner
getTypeHolesFromValue (TypedTypeHole _ typeHoleId) = [typeHoleId]
getTypeHolesFromValue (TypedLambda _ _ lambdaValue) = getTypeHolesFromValue lambdaValue
getTypeHolesFromValue (TypedReference _ _ args) = concatMap getTypeHolesFromValue args
getTypeHolesFromValue (TypedLiteral _ _) = []

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
      runFunction $ ffi $ "console.error('Failed to find type hole element with id " ++ typeHoleId ++ "')"
      return Nothing

createFunctionDroppedEvent :: Element -> String -> UI (Event FunctionDroppedEvent)
createFunctionDroppedEvent typeHoleElement holeId = do
  let dropEvent = UI.drop typeHoleElement
  runFunction $ ffi $ "console.log('create event for " ++ holeId ++ "')"
  return $ createFunctionDroppedEventFromDropEvent dropEvent holeId

createFunctionDroppedEventFromDropEvent :: Event UI.DragData -> String -> Event FunctionDroppedEvent
createFunctionDroppedEventFromDropEvent dropEvent holeId = fmap (FunctionDroppedEvent holeId) dropEvent

replaceTypeHoleWithValue :: String -> String -> TypedValue -> UI ValueDefinitionUpdateResult
replaceTypeHoleWithValue typedValueNameToInsert targetTypeHoleId definedValue = do
  runFunction $ ffi $ "console.log('working on function: " ++ show definedValue ++ ", dropped value: " ++ typedValueNameToInsert ++ ", into type hole: " ++ targetTypeHoleId ++ "')"
  let untypedValueResultAction
        | "literal-int" `isPrefixOf` typedValueNameToInsert = insertIntegerLiteralIntoValue targetTypeHoleId definedValue
        | "literal-string" `isPrefixOf` typedValueNameToInsert = insertStringLiteralIntoValue targetTypeHoleId definedValue
        | "literal-boolean" `isPrefixOf` typedValueNameToInsert = insertBooleanLiteralIntoValue typedValueNameToInsert targetTypeHoleId definedValue
        | "prelude-" `isPrefixOf` typedValueNameToInsert = return $ insertPreludeValueIntoValue typedValueNameToInsert targetTypeHoleId definedValue
        | "lambdaParam-" `isPrefixOf` typedValueNameToInsert = return $ insertLambdaParamIntoValue typedValueNameToInsert targetTypeHoleId definedValue
        | "definitionReference-" `isPrefixOf` typedValueNameToInsert = return $ insertDefinedValueIntoValue typedValueNameToInsert targetTypeHoleId definedValue
        | otherwise = return $ Left $ "Found unknown value name type " ++ typedValueNameToInsert
  untypedValueResult <- untypedValueResultAction
  case untypedValueResult of
    Right untypedValue -> do
      runFunction $ ffi $ "console.log('toinfer " ++ show untypedValue ++ "')"
      let inferenceResult = infere untypedValue
      runFunction $ ffi $ "console.log('infered " ++ show inferenceResult ++ "')"
      case inferenceResult of
        Success updatedValue -> return $ UpdateSuccess updatedValue
        Error e -> return $ UpdateError e
    Left e -> return $ UpdateError e

removePrefix :: String -> String -> String
removePrefix prefix original = fromMaybe original $ stripPrefix prefix original

insertBooleanLiteralIntoValue :: String -> String -> TypedValue -> UI (Either String UntypedValue)
insertBooleanLiteralIntoValue valueNameToInsert targetTypeHoleId definedValue = do
  let boolData = removePrefix "literal-boolean-" valueNameToInsert
  return $ case boolData of
    "true" -> Right $ insertUntypedValueIntoTypeHole (BooleanLiteral "True") targetTypeHoleId definedValue
    "false" -> Right $ insertUntypedValueIntoTypeHole (BooleanLiteral "False") targetTypeHoleId definedValue
    _ -> Left $ "Unknown boolean value '" ++ boolData ++ "'"

insertStringLiteralIntoValue :: String -> TypedValue -> UI (Either String UntypedValue)
insertStringLiteralIntoValue targetTypeHoleId definedValue = do
  stringLiteral <- liftIO generateRandomString
  let valueToInsert = StringLiteral $ Just ("\"" ++ stringLiteral ++ "\"")
  return $ Right $ insertUntypedValueIntoTypeHole valueToInsert targetTypeHoleId definedValue

insertIntegerLiteralIntoValue :: String -> TypedValue -> UI (Either String UntypedValue)
insertIntegerLiteralIntoValue targetTypeHoleId definedValue = do
  intLiteral <- liftIO generateRandomInt
  let valueToInsert = IntegerLiteral $ Just (show intLiteral)
  return $ Right $ insertUntypedValueIntoTypeHole valueToInsert targetTypeHoleId definedValue

insertPreludeValueIntoValue :: String -> String -> TypedValue -> Either String UntypedValue
insertPreludeValueIntoValue valueNameToInsert targetTypeHoleId definedValue = case getPreludeValue valueNameToInsert of
  Right preludeValue -> Right $ insertUntypedValueIntoTypeHole preludeValue targetTypeHoleId definedValue
  Left e -> Left e

getPreludeValue :: String -> Either String UntypedValue
getPreludeValue valueNameToInsert = do
  let preludeFunctionValue = removePrefix "prelude-" valueNameToInsert
  let maybePreludeValue = getValueFromPrelude preludeFunctionValue
  case maybePreludeValue of
    Just preludeValue -> Right preludeValue
    Nothing -> Left $ "Failed to locate prelude element " ++ preludeFunctionValue

insertDefinedValueIntoValue :: String -> String -> TypedValue -> Either String UntypedValue
insertDefinedValueIntoValue valueNameToInsert targetTypeHoleId definedValue = case getDefinedValue valueNameToInsert definedValue of
  Right valueReference -> Right $ insertUntypedValueIntoTypeHole valueReference targetTypeHoleId definedValue
  Left e -> Left e

getDefinedValue :: String -> TypedValue -> Either String UntypedValue
getDefinedValue valueNameToInsert definedValue = do
  let name = removePrefix "definitionReference-" valueNameToInsert
  let maybeDefinition = findDefinedValueInValue name definedValue
  case maybeDefinition of
    Just definitionType -> Right $ Reference (Just definitionType) name $ ToFill definitionType
    Nothing -> Left $ "The definition " ++ name ++ " doesn't exist"

insertLambdaParamIntoValue :: String -> String -> TypedValue -> Either String UntypedValue
insertLambdaParamIntoValue valueNameToInsert targetTypeHoleId definedValue = case getLambdaParameterValue valueNameToInsert definedValue of
  Right lambdaParam -> Right $ insertUntypedValueIntoTypeHole lambdaParam targetTypeHoleId definedValue
  Left e -> Left e

getLambdaParameterValue :: String -> TypedValue -> Either String UntypedValue
getLambdaParameterValue valueNameToInsert definedValue = do
  let lambdaParamName = removePrefix "lambdaParam-" valueNameToInsert
  let maybeLambdaParam = findLambdaParamInValue lambdaParamName definedValue
  case maybeLambdaParam of
    Just paramType -> Right $ Reference (Just paramType) lambdaParamName $ ToFill paramType
    Nothing -> Left $ "The parameter " ++ lambdaParamName ++ " doesn't exist"

getValueFromPrelude :: String -> Maybe UntypedValue
getValueFromPrelude name = find (isValueWithName name) $ concatMap WellKnown.values prelude

isValueWithName :: String -> UntypedValue -> Bool
isValueWithName name (Reference _ refName _) = refName == name
isValueWithName name (Lambda {}) = name == "lambda"
isValueWithName _ _ = False

findDefinedValueInValue :: String -> TypedValue -> Maybe Type
findDefinedValueInValue valueName (TypedValueDefinition typ name inner) = if name == valueName
  then Just typ
  else  findDefinedValueInValue valueName inner
findDefinedValueInValue valueName (TypedLambda _ _ lBody) = findDefinedValueInValue valueName lBody
findDefinedValueInValue valueName (TypedReference _ _ args) = if null args
  then Nothing
  else let results = mapMaybe (findLambdaParamInValue valueName) args in
    if null results
      then Nothing
      else Just $ head results
findDefinedValueInValue _ (TypedTypeHole _ _) = Nothing
findDefinedValueInValue _ (TypedLiteral _ _) = Nothing

findLambdaParamInValue :: String -> TypedValue -> Maybe Type
findLambdaParamInValue paramName (TypedLambda _ (lType, lName) lBody) = if paramName == lName
  then Just lType
  else findLambdaParamInValue paramName lBody
findLambdaParamInValue _ (TypedTypeHole _ _) = Nothing
findLambdaParamInValue paramName (TypedValueDefinition _ _ inner) =
  findLambdaParamInValue paramName inner
findLambdaParamInValue paramName (TypedReference _ _ args) = if null args
  then Nothing
  else let results = mapMaybe (findLambdaParamInValue paramName) args in
    if null results
      then Nothing
      else Just $ head results
findLambdaParamInValue _ (TypedLiteral _ _) = Nothing

generateRandomInt :: IO Int
generateRandomInt = randomRIO (1,1000)

generateRandomString :: IO String
generateRandomString = flip replicateM (randomRIO ('a','z')) =<< randomRIO (1,20)