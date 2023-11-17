module Frontend where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import FunctionEditor (generateComposedFunction)
import Functions (lookupFunction, functions)
import Model (Function, functionName)
import Data.Maybe (maybeToList)

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

    let functionUnderConstruction = lookupFunction "id1"
    _ <- element functionEditorContainer #+ maybeToList (renderFunction functionUnderConstruction)
    
    _ <- element sideBarContainer #+ renderSidebar (map snd functions)

    return ()

renderFunction :: Maybe Function -> Maybe (UI Element)
renderFunction (Just f) = Just $ generateComposedFunction f
renderFunction Nothing = Nothing

renderSidebar :: [Function] -> [UI Element]
renderSidebar = map renderSidebarFunctionBlock

renderSidebarFunctionBlock :: Function -> UI Element
renderSidebarFunctionBlock function = UI.div # set UI.text (functionName function)
                                             #. "sidebar-function-block"

createAppContainer :: UI Element
createAppContainer = UI.new # set UI.id_ "visual-fp-application-container"

createSideBarContainer :: UI Element
createSideBarContainer = UI.new # set UI.id_ "visual-fp-sidebar"

createFunctionEditorContainer :: UI Element
createFunctionEditorContainer = UI.new # set UI.id_ "function-editor-container"
