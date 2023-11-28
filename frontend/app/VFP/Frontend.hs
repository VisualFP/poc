-- Copyright (C) 2023 Lukas Streckeisen & Jann Flepp

module VFP.Frontend where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import VFP.FunctionEditor (FunctionDroppedEvent (..), generateValueDefinitionElement, getFunctionDroppedEvents, getTypeHolesFromValue, replaceTypeHoleWithValue, ValueDefinitionUpdateResult (..))
import Control.Monad (unless)
import VFP.UI.UIModel
import qualified VFP.Translation.WellKnown as WellKnown


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

  resetEditorAndRenderFunction window functionEditorContainer "test" WellKnown.string $ TypedTypeHole WellKnown.string "0"

  return ()

resetEditorAndRenderFunction :: Window -> Element -> String -> Type -> TypedValue -> UI ()
resetEditorAndRenderFunction window functionEditor defName defType valueDefinition = do
  runFunction $ ffi $ "console.log('render function: " ++ show valueDefinition ++ "')"
  resetEditor functionEditor
  runFunction $ ffi $ "console.log('got value " ++ show valueDefinition ++ "')"
  let functionElement = generateValueDefinitionElement defName defType valueDefinition
  _ <- element functionEditor #+ [functionElement]
  registerFunctionDroppedEvents window functionEditor defName defType valueDefinition
  return ()

resetEditor :: Element -> UI ()
resetEditor functionEditor = do
  _ <- element functionEditor # set children []
  return ()

registerFunctionDroppedEvents :: Window -> Element -> String -> Type -> TypedValue -> UI ()
registerFunctionDroppedEvents window functionEditor defName defType typedValue = do
  let typeHoles = getTypeHolesFromValue typedValue
  runFunction $ ffi $ "console.log('found " ++ show (length typeHoles) ++ " type holes')"
  events <- getFunctionDroppedEvents window typeHoles
  if not (null events)
    then do
      runFunction $ ffi $ "console.log('got " ++ show (length events) ++ " events')"
      mapM_ (registerFunctionDroppedEvent window functionEditor defName defType typedValue) events
    else do
      unless (null typeHoles) $ runFunction $ ffi "console.error('failed to define drop events')"

registerFunctionDroppedEvent :: Window -> Element -> String -> Type -> TypedValue -> Event FunctionDroppedEvent -> UI ()
registerFunctionDroppedEvent window functionEditor defName defType valueDefinition event = do
  _ <- onEvent event $ \dropEvent -> do
    runFunction $ ffi "console.log('dropped value')"
    updateResult <- replaceTypeHoleWithValue (functionDragData dropEvent) (functionDropTargetId dropEvent) valueDefinition
    case updateResult of
      UpdateSuccess updatedValueDefinition -> do
        resetEditorAndRenderFunction window functionEditor defName defType updatedValueDefinition
      UpdateError e -> do
        _ <- element functionEditor #+ [createErrorMessage e]
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
renderSidebarValueBlock IntegerLiteral = do
  literalElement <- UI.div #. "value literal literal-integer"
                    # set UI.draggable True
                    # set UI.dragData "literal-integer"
  _ <- element literalElement #+ [UI.p # set UI.text "Integer Literal"]
  literalTypeElement <- UI.p # set UI.text (show WellKnown.int)
                             #. "value-type"
  _ <- element literalElement #+ [element literalTypeElement]
  return literalElement
renderSidebarValueBlock StringLiteral = do
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

createErrorMessage :: String -> UI Element
createErrorMessage errorMessage = do
  errorElement <- UI.div #. "error-message"
  errorMessageElement <- UI.p # set UI.text errorMessage
  errorDismissButton <- UI.button # set UI.text "X"
  _ <- element errorElement #+ [element errorMessageElement, element errorDismissButton]
  on UI.click errorDismissButton $ \_ -> delete errorElement
  return errorElement