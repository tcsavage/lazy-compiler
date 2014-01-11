module GMachine.ViaC where

import Data.List
import Data.List.Utils
import Data.Maybe
import System.IO.Unsafe
import Text.Printf

import Paths_simplelang

import GMachine

-- | Given a list of (name, gcode, arity) triplets, build a C file which will run the code.
generateC :: [(String, [Instruction String], Int)] -> String
generateC globals = printf template (length globals) (concatMap translateGlobal $ renameGlobals globals)

-- | The template C file. (res/template.c)
template :: String
template = unsafePerformIO $ do
    template <- getDataFileName "template.c"
    readFile template

-- | Give each global name a numeric offset and replace instances of the name with the offset instead.
renameGlobals :: [(String, [Instruction String], Int)] -> [(String, Int, [Instruction Int], Int)]
renameGlobals globals = map doRename globals
    where
        names = map (\(name, ins, arity) -> name) globals
        mapper name = fromMaybe (error $ printf "Unknown global `%s`\n" name) $ lookup name $ zip names [0..]
        removeHash = replace "#" "_PRIM_"  -- Hashes are not valid in C identifiers.
        doRename (name, code, arity) = (removeHash name, mapper name, map (fmap mapper) code, arity)

endIns :: String
endIns = "{ .instType = INS_END, .arg = 0 }"

-- | Translate instructions into C values.
translateIns :: Instruction Int -> String
translateIns (PushGlobal n) = printf "{ .instType = INS_PUSHGLOBAL, .arg = %d }" n
translateIns (PushInt n) = printf "{ .instType = INS_PUSHINT, .arg = %d }" n
translateIns (Push n) = printf "{ .instType = INS_PUSH, .arg = %d }" n
translateIns MkAp = "{ .instType = INS_MKAP, .arg = 0 }"
translateIns (Update n) = printf "{ .instType = INS_UPDATE, .arg = %d }" n
translateIns (Slide n) = printf "{ .instType = INS_SLIDE, .arg = %d }" n
translateIns (Alloc n) = printf "{ .instType = INS_ALLOC, .arg = %d }" n
translateIns (Pop n) = printf "{ .instType = INS_POP, .arg = %d }" n
translateIns Unwind = "{ .instType = INS_UNWIND, .arg = 0 }"
translateIns Eval = "{ .instType = INS_EVAL, .arg = 0 }"
translateIns Add = "{ .instType = INS_ADD, .arg = 0 }"
translateIns Mul = "{ .instType = INS_MUL, .arg = 0 }"

-- | Translate GCode into a sequence of C values.
translateCode :: [Instruction Int] -> String
translateCode ins = (intercalate ", " $ map translateIns ins) ++ ", " ++ endIns

translateGlobal :: (String, Int, [Instruction Int], Int) -> String
translateGlobal (name, offset, code, arity) = unlines [commentLine, insDefLine, nodeAllocLine, tableInsLine]
    where
        insCount = length code + 1  -- +1 for the insEnd().
        commentLine = printf " // Global `%s`. Offset %d. Arity %d" name offset arity
        insDefLine = printf " Instruction global_%s_is[%d] = { %s };" name insCount (translateCode code)
        nodeAllocLine = printf " Node *global_%s_node = mkNodeGlobal(%d, global_%s_is);" name arity name
        tableInsLine = printf " gt[%d] = global_%s_node;" offset name
