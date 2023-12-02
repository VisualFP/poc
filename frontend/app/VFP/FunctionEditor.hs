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

data FunctionDroppedEvent = FunctionDroppedEvent
  { functionDropTargetId :: String,
    functionDragData :: UI.DragData
  }
  deriving (Show, Eq)

data ValueDefinitionUpdateResult
  = UpdateSuccess TypedValue
  | UpdateError String

generateValueDefinitionElement :: Identifier -> Type -> TypedValue -> UI Element
generateValueDefinitionElement defName defType valueDefinition = do
  valueDefinitionElement <- UI.new #. "value-definition function-editor-element"
  _ <- element valueDefinitionElement #+ [UI.p # set UI.text defName]
  _ <- element valueDefinitionElement #+ [generateValueElement valueDefinition]
  definitionTypeElement <-
    UI.p
      # set UI.text (printShortType defType)
      #. "definition-type"
  _ <- element valueDefinitionElement #+ [element definitionTypeElement]
  return valueDefinitionElement

generateValueElement :: TypedValue -> UI Element
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
      # set UI.text paramName
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

replaceTypeHoleWithValue :: String -> String -> ValueUnderConstruction -> UI ValueDefinitionUpdateResult
replaceTypeHoleWithValue valueNameToInsert targetTypeHoleId valueUnderConstruction = do
  runFunction $ ffi $ "console.log('working on function: " ++ show valueUnderConstruction ++ ", dropped value: " ++ valueNameToInsert ++ ", into type hole: " ++ targetTypeHoleId ++ "')"
  let definedValue = valueDefinition valueUnderConstruction
  let valueResultAction
        | "literal-int" `isPrefixOf` valueNameToInsert = insertIntegerLiteralIntoValue targetTypeHoleId definedValue
        | "literal-string" `isPrefixOf` valueNameToInsert = insertStringLiteralIntoValue targetTypeHoleId definedValue
        | "prelude-" `isPrefixOf` valueNameToInsert = return $ insertPreludeValueIntoValue valueNameToInsert targetTypeHoleId definedValue
        | "lambdaParam-" `isPrefixOf` valueNameToInsert = return $ insertLambdaParamIntoValue valueNameToInsert targetTypeHoleId definedValue
        | otherwise = return $ Left $ "Found unknown value name type " ++ valueNameToInsert
  valueResult <- valueResultAction
  case valueResult of
    Right untypedValue -> do
      runFunction $ ffi $ "console.log('toinfer " ++ show untypedValue ++ "')"
      let inferenceResult = infere $ UntypedValueUnderConstruction (valueType valueUnderConstruction) untypedValue
      runFunction $ ffi $ "console.log('infered " ++ show inferenceResult ++ "')"
      case inferenceResult of
        Success updatedValue -> return $ UpdateSuccess updatedValue
        Error e -> return $ UpdateError e
    Left e -> return $ UpdateError e

removePrefix :: String -> String -> String
removePrefix prefix original = fromMaybe original $ stripPrefix prefix original

insertStringLiteralIntoValue :: String -> TypedValue -> UI (Either String UntypedValue)
insertStringLiteralIntoValue targetTypeHoleId definedValue = do
  stringLiteral <- liftIO generateRandomString
  let valueToInsert = StringLiteral $ Just ("\"" ++ stringLiteral ++ "\"")
  return $ insertUntypedValueIntoTypeHole valueToInsert targetTypeHoleId definedValue

insertIntegerLiteralIntoValue :: String -> TypedValue -> UI (Either String UntypedValue)
insertIntegerLiteralIntoValue targetTypeHoleId definedValue = do
  intLiteral <- liftIO generateRandomInt
  let valueToInsert = IntegerLiteral $ Just (show intLiteral)
  return $ insertUntypedValueIntoTypeHole valueToInsert targetTypeHoleId definedValue

insertPreludeValueIntoValue :: String -> String -> TypedValue -> Either String UntypedValue
insertPreludeValueIntoValue valueNameToInsert targetTypeHoleId definedValue = case getPreludeValue valueNameToInsert of
  Right preludeValue -> insertUntypedValueIntoTypeHole preludeValue targetTypeHoleId definedValue
  Left e -> Left e

getPreludeValue :: String -> Either String UntypedValue
getPreludeValue valueNameToInsert = do
  let preludeFunctionValue = removePrefix "prelude-" valueNameToInsert
  let maybePreludeValue = getValueFromPrelude preludeFunctionValue
  case maybePreludeValue of
    Just preludeValue -> Right preludeValue
    Nothing -> Left $ "Failed to locate prelude element " ++ preludeFunctionValue

insertLambdaParamIntoValue :: String -> String -> TypedValue -> Either String UntypedValue
insertLambdaParamIntoValue valueNameToInsert targetTypeHoleId definedValue = case getLambdaParameterValue valueNameToInsert definedValue of
  Right lambdaParam -> insertUntypedValueIntoTypeHole lambdaParam targetTypeHoleId definedValue
  Left e -> Left e

getLambdaParameterValue :: String -> TypedValue -> Either String UntypedValue
getLambdaParameterValue valueNameToInsert definedValue = do
  let lambdaParamName = removePrefix "lambdaParam-" valueNameToInsert
  let maybeLambdaParam = findLambdaParamInValue lambdaParamName definedValue
  case maybeLambdaParam of
    Just (paramType, lambdaParam) -> Right $ Reference (Just paramType) lambdaParam $ ToFill paramType
    Nothing -> Left $ "The parameter " ++ lambdaParamName ++ " doesn't exist"

getValueFromPrelude :: String -> Maybe UntypedValue
getValueFromPrelude name = find (isValueWithName name) $ concatMap WellKnown.values prelude

isValueWithName :: String -> UntypedValue -> Bool
isValueWithName name (Reference _ refName _) = refName == name
isValueWithName name (Lambda {}) = name == "lambda"
isValueWithName _ _ = False

findLambdaParamInValue :: String -> TypedValue -> Maybe (Type, Identifier)
findLambdaParamInValue paramName (TypedLambda _ (lType, lName) lBody) = if paramName == lName
  then Just (lType, lName)
  else findLambdaParamInValue paramName lBody
findLambdaParamInValue _ (TypedTypeHole _ _) = Nothing
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