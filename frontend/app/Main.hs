-- Copyright (C) 2023 Lukas Streckeisen & Jann Flepp

module Main where

import System.Environment ( getArgs )
import VFP.Frontend (start)

main :: IO ()
main = do
    [port, dir] <- getArgs
    start (read port) dir
