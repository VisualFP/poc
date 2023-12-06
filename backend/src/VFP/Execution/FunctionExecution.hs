module VFP.Execution.FunctionExecution where

import System.Directory (createDirectoryIfMissing, setCurrentDirectory)
import System.FilePath.Posix (takeDirectory)
import System.Process
import System.Exit

executionDirectory :: String
executionDirectory = "../build/"

executableName :: String
executableName = "visual-fp-execution"

preludeDefinitions :: String
preludeDefinitions = "boolToString :: Bool -> String\n\
                     \boolToString b = show b"

writeHaskellFile :: String -> String -> IO ()
writeHaskellFile mainFunctionDefinition functionDefinitions = do
    let fileContent = "module Main where\n\n\
        \main :: IO ()\n\
        \main = do\n\
        \   putStrLn " ++ mainFunctionDefinition ++ "\n\
        \   return ()\n\n\
        \" ++ functionDefinitions ++ "\n\n\
        \" ++ preludeDefinitions
    let mainFile = executionDirectory ++ "Main.hs"
    createPathAndWriteFile mainFile fileContent

writeCabalFile :: IO ()
writeCabalFile = do
    let cabalFile = executionDirectory ++ executableName ++ ".cabal"
    let cabalFileContent = "cabal-version:      3.0\n\
        \name:              " ++ executableName ++ "\n\
        \version:           1.0.0.0\n\
        \build-type:        Simple\n\n\
        \executable " ++ executableName ++ "\n\
        \   main-is:            Main.hs\n\
        \   build-depends:      base >= 4.18 && < 5\n\
        \   ghc-options:        -Wall\n\
        \   default-language:   Haskell2010\n"
    
    createPathAndWriteFile cabalFile cabalFileContent
    return ()

createPathAndWriteFile :: String -> String -> IO ()
createPathAndWriteFile path content = do
    createDirectoryIfMissing True $ takeDirectory path
    writeFile path content
    return ()

executeFunction :: String -> String -> IO (Either String String)
executeFunction mainFunctionDefinition functionDefinitions = do
    writeHaskellFile mainFunctionDefinition functionDefinitions
    writeCabalFile
    setCurrentDirectory executionDirectory
    (buildExitCode, _, _) <- readProcessWithExitCode "cabal" ["build"] ""
    case buildExitCode of
        ExitSuccess -> do
            (runExitCode, runOutput, _) <- readProcessWithExitCode "cabal" ["run"] ""
            case runExitCode of
                ExitSuccess -> return $ Right runOutput
                (ExitFailure _) -> return $ Left "Execution failed"
        (ExitFailure _) -> return $ Left "Build failed"
