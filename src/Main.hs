module Main where

import Control.Applicative
import qualified Data.ByteString as B
import Data.Char
import Data.String
import System.Environment
import Text.Printf

import AST
import Parsec
import Compiler
import GMachine
import GMachine.ViaC

main :: IO ()
main = do
    [backend, srcFile] <- getArgs
    ast <- genAST <$> readFile srcFile
    case backend of
        "-bviac" -> writeFile (srcFile++".c") $ generateC $ map compileTopLevel ast
        _ -> error "Unsupported backend."

genAST :: String -> [(Ident, Expr Ident)]
genAST file = [unDecl $ parse $ fromString l  | l <- lines file, not (all isSpace l || null l)]
    where
        parse line = case parseSource line of
            Left e -> error $ printf "Parser error on line: \"%s\"" line
            Right d -> d
