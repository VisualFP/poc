module Frontend where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import FunctionEditor (generateComposedFunction, FunctionDroppedEvent (..), getTypeHolesFromFunction, getFunctionDroppedEvents, replaceTypeHoleWithFunction)
import Functions (lookupFunction, functions)
import Model (Function (functionId, definition), functionName)

start :: Int -> String -> IO()
start port dir = startGUI defaultConfig
    {
        jsPort = Just port,
        jsStatic = Just $ dir ++ "/bin/static"
    } $ \window -> do
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

    resetEditorAndRenderFunction window functionEditorContainer "id1"
    _ <- element sideBarContainer #+ renderSidebar (map snd functions)

    return ()

resetEditorAndRenderFunction :: Window -> Element -> String -> UI ()
resetEditorAndRenderFunction window functionEditor funcId = do
    runFunction $ ffi $ "console.log('render function: " ++ funcId ++ "')"
    resetEditor functionEditor
    let maybeFunction = lookupFunction funcId
    case maybeFunction of
        Just function -> do
            let functionElement = generateComposedFunction function
            _ <- element functionEditor #+ [functionElement]
            registerFunctionDroppedEvent window functionEditor function
            return ()
        Nothing -> return ()

resetEditor :: Element -> UI ()
resetEditor functionEditor = do
    _ <- element functionEditor # set children []
    return ()

renderFunction :: Maybe Function -> Maybe (UI Element)
renderFunction (Just f) = Just $ generateComposedFunction f
renderFunction Nothing = Nothing

registerFunctionDroppedEvent :: Window -> Element -> Function -> UI ()
registerFunctionDroppedEvent window functionEditor function = do
    let typeHoles = getTypeHolesFromFunction $ definition function
    maybeEvent <- getFunctionDroppedEvents window typeHoles
    case maybeEvent of
        Just event -> do
            _ <- onEvent event $ \es -> do
                runFunction $ ffi "console.log('Hi Event')"
                mapM_ (\e -> replaceTypeHoleWithFunction (functionId function) (functionDragData e) (functionDropTargetId e)) es
                resetEditorAndRenderFunction window functionEditor "id2"
            return ()
        Nothing -> do
            return ()


renderSidebar :: [Function] -> [UI Element]
renderSidebar = map renderSidebarFunctionBlock

renderSidebarFunctionBlock :: Function -> UI Element
renderSidebarFunctionBlock function = UI.div # set UI.text (functionName function)
                                             #. "sidebar-function-block"
                                             # set UI.draggable True
                                             # set UI.dragData (functionId function)

createAppContainer :: UI Element
createAppContainer = UI.new # set UI.id_ "visual-fp-application-container"

createSideBarContainer :: UI Element
createSideBarContainer = UI.new # set UI.id_ "visual-fp-sidebar"

createFunctionEditorContainer :: UI Element
createFunctionEditorContainer = UI.new # set UI.id_ "function-editor-container"