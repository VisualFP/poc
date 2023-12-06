-- Copyright (C) 2023 Lukas Streckeisen & Jann Flepp

module VFP.Frontend where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import VFP.FunctionEditor (FunctionDroppedEvent (..), getFunctionDroppedEvents, getTypeHolesFromValue, replaceTypeHoleWithValue, ValueDefinitionUpdateResult (..), generateValueElement)
import Control.Monad (unless)
import VFP.UI.UIModel
import qualified VFP.Translation.WellKnown as WellKnown
import Data.Char (toLower)
import VFP.Translation.TranslateToHaskellSource (translateToHaskellCode)

start :: Int -> String -> IO ()
start port dir = startGUI
  defaultConfig
    { jsPort = Just port,
      jsStatic = Just dir
    }
  $ \window -> do
    _ <- return window # set UI.title "VisualFP"
    UI.addStyleSheet window "visualFP.css"
    setup window

setup :: Window -> UI ()
setup window = do
  appContainer <- createAppContainer

  sideBarContainer <- createSideBarContainer
  functionEditorContainer <- createFunctionEditorContainer

  _ <- element appContainer #+ [element sideBarContainer, element functionEditorContainer]
  _ <- getBody window #+ [element appContainer]
  _ <- element sideBarContainer #+ renderSidebar WellKnown.prelude

  let valueUnderConstruction = TypedValueDefinition (Generic 1) "userDefinedFunction" $ TypedTypeHole (Generic 1) "0"

  resetButton <- createEditorResetButton
  _ <- element appContainer #+ [element resetButton]
  on UI.click resetButton $ \_ -> resetEditorAndRenderFunction window functionEditorContainer valueUnderConstruction

  resetEditorAndRenderFunction window functionEditorContainer valueUnderConstruction

resetEditorAndRenderFunction :: Window -> Element -> TypedValue -> UI ()
resetEditorAndRenderFunction window functionEditor valueUnderConstruction = do
  runFunction $ ffi $ "console.log('render function: " ++ show valueUnderConstruction ++ "')"
  resetEditor functionEditor
  runFunction $ ffi $ "console.log('got value " ++ show valueUnderConstruction ++ "')"
  let functionElement = generateValueElement valueUnderConstruction
  _ <- element functionEditor #+ [functionElement]
  registerFunctionDroppedEvents window functionEditor valueUnderConstruction

  sourceButton <- UI.button # set UI.id_ "view-haskell-button"
                            # set UI.text "View Haskell"
  _ <- element functionEditor #+ [element sourceButton]
  on UI.click sourceButton $ \_ -> onViewSourceButtonClicked functionEditor valueUnderConstruction

  executeButton <- UI.button # set UI.id_ "execute-button"
                             # set UI.text "Execute Function"
  _ <- element functionEditor #+ [element executeButton]
  on UI.click executeButton $ \_ -> onExecuteButtonClicked functionEditor valueUnderConstruction

resetEditor :: Element -> UI ()
resetEditor functionEditor = do
  _ <- element functionEditor # set children []
  return ()

registerFunctionDroppedEvents :: Window -> Element -> TypedValue -> UI ()
registerFunctionDroppedEvents window functionEditor valueUnderConstruction = do
  let typeHoles = getTypeHolesFromValue valueUnderConstruction
  runFunction $ ffi $ "console.log('found " ++ show (length typeHoles) ++ " type holes')"
  events <- getFunctionDroppedEvents window typeHoles
  if not (null events)
    then do
      runFunction $ ffi $ "console.log('got " ++ show (length events) ++ " events')"
      mapM_ (registerFunctionDroppedEvent window functionEditor valueUnderConstruction) events
    else do
      unless (null typeHoles) $ runFunction $ ffi "console.error('failed to define drop events')"

registerFunctionDroppedEvent :: Window -> Element -> TypedValue -> Event FunctionDroppedEvent -> UI ()
registerFunctionDroppedEvent window functionEditor valueUnderConstruction event = do
  _ <- onEvent event $ \dropEvent -> do
    runFunction $ ffi "console.log('dropped value')"
    updateResult <- replaceTypeHoleWithValue (functionDragData dropEvent) (functionDropTargetId dropEvent) valueUnderConstruction
    case updateResult of
      UpdateSuccess updatedValueDefinition -> do
        resetEditorAndRenderFunction window functionEditor updatedValueDefinition
      UpdateError e -> do
        (popup, _) <- createPopup "Error" e "error-message"
        _ <- element functionEditor #+ [element popup]
        runFunction $ ffi $ "console.error('Failed to update function: " ++ e ++ "')"
    return ()
  return ()

renderSidebar :: [WellKnown.PreludeGroup] -> [UI Element]
renderSidebar = concatMap renderSidebarGroupBlock

renderSidebarGroupBlock :: WellKnown.PreludeGroup -> [UI Element]
renderSidebarGroupBlock WellKnown.PreludeGroup{WellKnown.name=name, WellKnown.values=values} =
  (UI.h2 # set UI.text name) : map renderSidebarValueBlock values

renderSidebarValueBlock :: UntypedValue -> UI Element
renderSidebarValueBlock (Reference refType refName _) = do
  preludeFunctionElement <- UI.div #. "value prelude-reference"
                                   # set UI.draggable True
                                   # set UI.dragData ("prelude-" ++ refName)
  _ <- element preludeFunctionElement #+ [UI.p # set UI.text refName]
  preludeFunctionTypeElement <- UI.p # set UI.text (maybe "" printFullType refType)
                                     #. "value-type"
  _ <- element preludeFunctionElement #+ [element preludeFunctionTypeElement]
  return preludeFunctionElement
renderSidebarValueBlock (IntegerLiteral _) = do
  literalElement <- UI.div #. "value literal literal-integer"
                    # set UI.draggable True
                    # set UI.dragData "literal-integer"
  _ <- element literalElement #+ [UI.p # set UI.text "Integer Literal"]
  literalTypeElement <- UI.p # set UI.text (show WellKnown.int)
                             #. "value-type"
  _ <- element literalElement #+ [element literalTypeElement]
  return literalElement
renderSidebarValueBlock (BooleanLiteral val) = do
  literalElement <- UI.div #. "value literal literal-boolean"
                    # set UI.draggable True
                    # set UI.dragData ("literal-boolean-" ++ map toLower val)
  _ <- element literalElement #+ [UI.p # set UI.text val]
  literalTypeElement <- UI.p # set UI.text (show WellKnown.bool)
                             #. "value-type"
  _ <- element literalElement #+ [element literalTypeElement]
  return literalElement
renderSidebarValueBlock (StringLiteral _) = do
  literalElement <- UI.div #. "value literal literal-string"
                    # set UI.draggable True
                    # set UI.dragData "literal-string"
  _ <- element literalElement #+ [UI.p # set UI.text "String Literal"]
  literalTypeElement <- UI.p # set UI.text (show WellKnown.string)
                             #. "value-type"
  _ <- element literalElement #+ [element literalTypeElement]
  return literalElement
renderSidebarValueBlock (Lambda lambdaType _) = do
  lambdaElement <- UI.div #. "value prelude-lambda"
                          # set UI.draggable True
                          # set UI.dragData "prelude-lambda"
  _ <- element lambdaElement #+ [UI.p # set UI.text "Lambda Function"]
  lambdaTypeElement <- UI.p # set UI.text (maybe "" printFullType lambdaType)
                            #. "value-type"
  _ <- element lambdaElement #+ [element lambdaTypeElement]
  return lambdaElement
renderSidebarValueBlock _ = UI.div # set UI.text "Unsupported prelude value"

createAppContainer :: UI Element
createAppContainer = UI.new # set UI.id_ "visual-fp-application-container"

createSideBarContainer :: UI Element
createSideBarContainer = UI.new # set UI.id_ "visual-fp-sidebar"

createFunctionEditorContainer :: UI Element
createFunctionEditorContainer = UI.new # set UI.id_ "function-editor-container"

createPopup :: String -> String -> String -> UI (Element, Event ())
createPopup popupTitle content popupType = do
  popupElement <- UI.div #. ("popup " ++ popupType)
  titleElement <- UI.p # set UI.text popupTitle
                       #. "popup-title"
  contentElement <- UI.p # set UI.text content
                         #. "popup-content"
  dismissButton <- UI.button # set UI.text "X"
  _ <- element popupElement #+ [element titleElement, element dismissButton, element contentElement]
  let dismissEvent = UI.click dismissButton
  _ <- onEvent dismissEvent $ \_ -> delete popupElement
  return (popupElement, dismissEvent)

createEditorResetButton :: UI Element
createEditorResetButton = UI.button # set UI.id_ "reset-button"
                                 # set UI.text "Reset Editor"

onViewSourceButtonClicked :: Element -> TypedValue -> UI ()
onViewSourceButtonClicked functionEditor typedValue = do
  let typeHoles = getTypeHolesFromValue typedValue
  if null typeHoles
    then do
      overlay <- UI.div #. "popup-overlay"
      let haskellCode = translateToHaskellCode typedValue
      (popup, dismissEvent) <- createPopup "Haskell Code" haskellCode "view-haskell-popup"
      
      _ <- element overlay #+ [element popup]
      _ <- element functionEditor #+ [element overlay]
      _ <- onEvent dismissEvent $ \_ -> delete overlay
      return ()
    else do
      (popup, _) <- createPopup "Error" "Cannot show Haskell code if function isn't fully defined" "error-message"
      _ <- element functionEditor #+ [element popup]
      return ()

onExecuteButtonClicked :: Element -> TypedValue -> UI ()
onExecuteButtonClicked functionEditor typedValue = do
  let typeHoles = getTypeHolesFromValue typedValue
  if null typeHoles
    then do
      if canExecuteValue typedValue
        then do
          let haskellCode = translateToHaskellCode typedValue
          executionResult <- liftIO $ executeFunction "userDefinedFunction" haskellCode
          case executionResult of
            (Left errorMessage) -> do
              (errorPopup, _) <- createPopup "Execution Error" errorMessage "error-message"
              _ <- element functionEditor #+ [element errorPopup]
              return ()
            (Right result) -> do
              (resultPopup, _) <- createPopup "Execution Result" result "execution-result"
              _ <- element functionEditor #+ [element resultPopup]
              return ()
        else do
          (popup, _) <- createPopup "Execution Error" "The defined value currently is not supported" "error-message"
          _ <- element functionEditor #+ [element popup]
          return ()
    else do
      (popup, _) <- createPopup "Execution Error" "Cannot execute function that isn't fully defined" "error-message"
      _ <- element functionEditor #+ [element popup]
      return ()

canExecuteValue :: TypedValue -> Bool
canExecuteValue (TypedValueDefinition (Primitive "String") _ _) = True
canExecuteValue _ = False