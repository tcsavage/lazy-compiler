module Main where

import System.Environment

import AST
import Compiler
import GMachine
import GMachine.ViaC

main :: IO ()
main = do
    [backend] <- getArgs
    case backend of
        "-bviac" -> writeFile "test.c" $ generateC $ map compileTopLevel testTopLevels
        _ -> error "Unsupported backend."
