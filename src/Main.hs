module Main where

import Control.Applicative
import qualified Data.ByteString as B
import Data.Char
import Data.String
import System.Environment
import Text.Printf
import Text.Show.Pretty

import Core.AST
import Core.Parsec
import Core.PrettyPrinter
import Core.TypeCheck
import STG.FromCore
import STG.PrettyPrint
import STG.ViaC

main :: IO ()
main = do
    [backend, srcFile] <- getArgs
    mod <- (typecheck . genAST) <$> readFile srcFile
    case backend of
        "-bviac" -> writeFile (srcFile++".c") $ compile $ core2stg mod
        "-bpp" -> putStrLn $ pp mod
        "-bppstg" -> putStrLn $ ppStg $ core2stg mod
        _ -> error "Unsupported backend."

-- | Parse each line as a global definition.
genAST :: String -> Module
genAST file = case parseSource file of
    Left e -> error $ printf "Parser error: %s" (show e)
    Right m -> m
