module VFP.Frontend where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import VFP.FunctionEditor (FunctionDroppedEvent (..), generateValueDefinitionElement, getFunctionDroppedEvents, getTypeHolesFromValue, replaceTypeHoleWithTypedValue, ValueDefinitionUpdateResult (..))
import Control.Monad (unless)
import VFP.UI.UIModel
import qualified VFP.Translation.WellKnown as WellKnown


start :: Int -> String -> IO ()
start port dir = startGUI
  defaultConfig
    { jsPort = Just port,
      --jsStatic = Just $ dir ++ "/bin/static"
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

  resetEditorAndRenderFunction window functionEditorContainer "test" WellKnown.string $ TypedTypeHole WellKnown.string "0"
  _ <- element sideBarContainer #+ renderSidebar WellKnown.prelude

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
    updateResult <- replaceTypeHoleWithTypedValue (functionDragData dropEvent) (functionDropTargetId dropEvent) valueDefinition
    case updateResult of
      UpdateSuccess updatedValueDefinition -> do
        resetEditorAndRenderFunction window functionEditor defName defType updatedValueDefinition
      UpdateError e -> do
        runFunction $ ffi $ "console.error('Failed to update function: " ++ e ++ ")"
    return ()
  return ()

renderSidebar :: [TypedValue] -> [UI Element]
renderSidebar = map renderSidebarFunctionBlock

renderSidebarFunctionBlock :: TypedValue -> UI Element
renderSidebarFunctionBlock (TypedReference refType refName _) = do
  preludeFunctionElement <- UI.div #. "prelude-function"
                                   # set UI.draggable True
                                   # set UI.dragData refName
  _ <- element preludeFunctionElement #+ [UI.p # set UI.text refName]
  preludeFunctionTypeElement <- UI.p # set UI.text (show refType)
                                     #. "function-type"
  _ <- element preludeFunctionElement #+ [element preludeFunctionTypeElement]
  return preludeFunctionElement
renderSidebarFunctionBlock _ = UI.div # set UI.text "Unsupported prelude value"


createAppContainer :: UI Element
createAppContainer = UI.new # set UI.id_ "visual-fp-application-container"

createSideBarContainer :: UI Element
createSideBarContainer = UI.new # set UI.id_ "visual-fp-sidebar"

createFunctionEditorContainer :: UI Element
createFunctionEditorContainer = UI.new # set UI.id_ "function-editor-container"
