module GMachine.ViaC where

import Data.List
import Data.Maybe
import System.IO.Unsafe
import Text.Printf

import GMachine

generateC :: [(String, [Instruction String], Int)] -> String
generateC globals = printf template (length globals) (concatMap translateGlobal $ renameGlobals globals)

template :: String
template = unsafePerformIO $ readFile "template.c"

-- | Give each global a numeric offset and replace instances of the number with the offset instead.
renameGlobals :: [(String, [Instruction String], Int)] -> [(String, Int, [Instruction Int], Int)]
renameGlobals globals = map doRename globals
    where
        names = map (\(name, ins, arity) -> name) globals
        mapper name = fromMaybe (error $ printf "Unknown global `%s`\n" name) $ lookup name $ zip names [0..]
        doRename (name, code, arity) = (name, mapper name, map (fmap mapper) code, arity)

translateIns :: Instruction Int -> String
translateIns (PushGlobal n) = printf "insPushGlobal(%d)" n
translateIns (PushInt n) = printf "insPushInt(%d)" n
translateIns (Push n) = printf "insPush(%d)" n
translateIns MkAp = "insMkAp()"
translateIns (Slide n) = printf "insSlide(%d)" n
translateIns Unwind = "insUnwind()"

translateCode :: [Instruction Int] -> String
translateCode ins = (intercalate ", " $ map translateIns ins) ++ ", insEnd()"

translateGlobal :: (String, Int, [Instruction Int], Int) -> String
translateGlobal (name, offset, code, arity) = unlines [commentLine, insDefLine, nodeAllocLine, tableInsLine]
    where
        insCount = length code + 1  -- +1 for the insEnd().
        commentLine = printf " // Global `%s`. Offset %d. Arity %d" name offset arity
        insDefLine = printf " Instruction global_%s_is[%d] = { %s };" name insCount (translateCode code)
        nodeAllocLine = printf " Node *global_%s_node = mkNodeGlobal(%d, global_%s_is);" name arity name
        tableInsLine = printf " globalTable[%d] = global_%s_node;" offset name
