module VFP.Frontend where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import VFP.FunctionEditor (FunctionDroppedEvent (..), generateValueDefinitionElement, getFunctionDroppedEvents, getTypeHolesFromValue)
import VFP.UI.Functions (functions, lookupFunction)
import VFP.UI.UIModel
  ( InferenceResult (..),
    TypedValue (..),
    UntypedValue,
    ValueDefinition (definitionName, definitionType, definitionValue),
  )

start :: Int -> String -> IO ()
start port dir = startGUI
  defaultConfig
    { jsPort = Just port,
      jsStatic = Just $ dir ++ "/bin/static"
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

  resetEditorAndRenderFunction window functionEditorContainer "id4"
  _ <- element sideBarContainer #+ renderSidebar (map snd functions)

  return ()

resetEditorAndRenderFunction :: Window -> Element -> String -> UI ()
resetEditorAndRenderFunction window functionEditor funcId = do
  runFunction $ ffi $ "console.log('render function: " ++ funcId ++ "')"
  resetEditor functionEditor
  let maybeFunction = lookupFunction funcId
  case maybeFunction of
    Just valueDefinition -> do
      let inferenceResult = infer (definitionValue valueDefinition)
      case inferenceResult of
        Error e -> do
          runFunction $ ffi $ "console.log('" ++ e ++ "')"
        Success typedValue -> do
          let functionElement = generateValueDefinitionElement (definitionName valueDefinition) (definitionType valueDefinition) typedValue
          _ <- element functionEditor #+ [functionElement]
          registerFunctionDroppedEvents window functionEditor typedValue
      return ()
    Nothing -> return ()

resetEditor :: Element -> UI ()
resetEditor functionEditor = do
  _ <- element functionEditor # set children []
  return ()

registerFunctionDroppedEvents :: Window -> Element -> TypedValue -> UI ()
registerFunctionDroppedEvents window functionEditor typedValue = do
  let typeHoles = getTypeHolesFromValue typedValue
  runFunction $ ffi $ "console.log('found " ++ show (length typeHoles) ++ " type holes')"
  maybeEvents <- getFunctionDroppedEvents window typeHoles
  case maybeEvents of
    Just events -> do
      mapM_ (registerFunctionDroppedEvent window functionEditor typedValue) events
    Nothing -> do
      return ()

registerFunctionDroppedEvent :: Window -> Element -> TypedValue -> Event FunctionDroppedEvent -> UI ()
registerFunctionDroppedEvent window functionEditor typedValue event = do
  _ <- onEvent event $ \dropEvent -> do
    -- replaceTypeHoleWithFunction (show $ referenceId $ definitionReference definition) (functionDragData dropEvent) (functionDropTargetId dropEvent)
    resetEditorAndRenderFunction window functionEditor "id4"
    return ()
  return ()

renderSidebar :: [ValueDefinition] -> [UI Element]
renderSidebar = map renderSidebarFunctionBlock

renderSidebarFunctionBlock :: ValueDefinition -> UI Element
renderSidebarFunctionBlock definition =
  UI.div
    # set UI.text (definitionName definition)
    #. "sidebar-function-block"
    # set UI.draggable True
    # set UI.dragData (definitionName definition)

createAppContainer :: UI Element
createAppContainer = UI.new # set UI.id_ "visual-fp-application-container"

createSideBarContainer :: UI Element
createSideBarContainer = UI.new # set UI.id_ "visual-fp-sidebar"

createFunctionEditorContainer :: UI Element
createFunctionEditorContainer = UI.new # set UI.id_ "function-editor-container"

infer :: UntypedValue -> InferenceResult
infer untyped = Success (TypedLambda "String -> Int -> String" ("String", "s") (TypedReference "String -> Int -> String" "functionTwo" [TypedReference "String" "s" [], TypedTypeHole "Int" "127"]))