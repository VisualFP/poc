module Main where

import System.Environment (getArgs)
import Frontend (start)

main :: IO ()
main = do
    [port] <- getArgs
    start (read port)
