module Main where

import System.Environment ( getArgs )
import Frontend (start)

main :: IO ()
main = do
    [port, dir] <- getArgs
    start (read port) dir
