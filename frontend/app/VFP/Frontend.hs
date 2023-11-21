module VFP.Frontend where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import VFP.FunctionEditor (FunctionDroppedEvent (..), getFunctionDroppedEvents, replaceTypeHoleWithFunction, getTypeHolesFromValue, generateValueDefinitionElement)
import VFP.UI.Functions (lookupFunction, functions)
import VFP.UI.UIModel ( ValueDefinition(definitionReference, definitionValue), ValueReference (referenceId, referenceName) )

import qualified Data.Map.Strict as Map

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
            let functionElement = generateValueDefinitionElement function Map.empty
            _ <- element functionEditor #+ [functionElement]
            registerFunctionDroppedEvents window functionEditor function
            return ()
        Nothing -> return ()

resetEditor :: Element -> UI ()
resetEditor functionEditor = do
    _ <- element functionEditor # set children []
    return ()

registerFunctionDroppedEvents :: Window -> Element -> ValueDefinition -> UI ()
registerFunctionDroppedEvents window functionEditor definition = do
    let typeHoles = getTypeHolesFromValue $ definitionValue definition
    runFunction $ ffi $ "console.log('found " ++ show (length typeHoles) ++ " type holes')"
    maybeEvents <- getFunctionDroppedEvents window typeHoles
    case maybeEvents of
        Just events -> do
            mapM_ (registerFunctionDroppedEvent window functionEditor definition) events
        Nothing -> do
            return ()

registerFunctionDroppedEvent :: Window -> Element -> ValueDefinition -> Event FunctionDroppedEvent -> UI ()
registerFunctionDroppedEvent window functionEditor definition event = do
    _ <- onEvent event $ \dropEvent -> do
        replaceTypeHoleWithFunction (show $ referenceId $ definitionReference definition) (functionDragData dropEvent) (functionDropTargetId dropEvent)
        resetEditorAndRenderFunction window functionEditor "id4"
        return ()
    return ()

renderSidebar :: [ValueDefinition] -> [UI Element]
renderSidebar = map renderSidebarFunctionBlock

renderSidebarFunctionBlock :: ValueDefinition -> UI Element
renderSidebarFunctionBlock definition = UI.div # set UI.text (referenceName $ definitionReference definition)
                                             #. "sidebar-function-block"
                                             # set UI.draggable True
                                             # set UI.dragData (show $ referenceId $ definitionReference definition)

createAppContainer :: UI Element
createAppContainer = UI.new # set UI.id_ "visual-fp-application-container"

createSideBarContainer :: UI Element
createSideBarContainer = UI.new # set UI.id_ "visual-fp-sidebar"

createFunctionEditorContainer :: UI Element
createFunctionEditorContainer = UI.new # set UI.id_ "function-editor-container"