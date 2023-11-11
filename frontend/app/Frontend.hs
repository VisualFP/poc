module Frontend where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import System.Environment (getExecutablePath)
import System.FilePath (dropFileName)
import Control.Monad.IO.Class (liftIO)
-- import MyLib (someFunc)

start :: Int -> IO()
start port = do
    execPath <- dropFileName <$> getExecutablePath
    startGUI defaultConfig
        {
            jsPort = Just port,
            jsStatic = Just $ execPath ++ "/static"
        } setup

setup :: Window -> UI()
setup window = do
    _ <- return window # set UI.title "VisualFP"
    button <- UI.button #set UI.text "HI"

    _ <- getBody window #+ [element button]

    on UI.click button $ const $ do
        runFunction $ ffi "alert('HI')"