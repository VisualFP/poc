module Frontend where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
-- import MyLib (someFunc)

start :: Int -> [Char] -> IO()
start port dir = do
    startGUI defaultConfig
        {
            jsPort = Just port,
            jsStatic = Just $ dir ++ "/bin/static"
        } setup

setup :: Window -> UI()
setup window = do
    _ <- return window # set UI.title "VisualFP"
    UI.addStyleSheet window "test.css"
    button <- UI.button #set UI.text "HI"

    _ <- getBody window #+ [element button]

    on UI.click button $ const $ do
        runFunction $ ffi "alert('HI')"