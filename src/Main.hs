module Main where

import Control.Applicative
import qualified Data.ByteString as B
import Data.Char
import Data.String
import System.Environment
import Text.Printf
import Text.Show.Pretty

import AST
import Compiler
import GMachine
import GMachine.Interpreter
import GMachine.ViaC
import Parsec
import PrettyPrinter
import TypeCheck

main :: IO ()
main = do
    [backend, srcFile] <- getArgs
    mod <- (checkModule . Module srcFile . genAST) <$> readFile srcFile
    case backend of
        "-bviac" -> writeFile (srcFile++".c") $ generateC $ compile mod
        "-bpp" -> putStrLn $ pp mod
        "-bppgc" -> putStrLn $ ppShow $ compile mod
        "-bi" -> do
            s0 <- mkState $ compile mod
            out <- runGCodeToValue s0
            case out of
                Nothing -> error "Output is not a value."
                Just x -> print x
        "-biv" -> do
            s0 <- mkState $ compile mod
            runGCodeVerbose s0
        _ -> error "Unsupported backend."

-- | Parse each line as a global definition.
genAST :: String -> [(Ident, Expr Ident)]
genAST file = [unDecl $ parse $ fromString l | l <- lines file, not (all isSpace l || null l)]
    where
        parse line = case parseSource line of
            Left e -> error $ printf "Parser error on line: \"%s\"" line
            Right d -> d
