{-# LANGUAGE PackageImports #-}

{-
Instead of generating C code directly, the STG compiler will instead build *abstract* C, a data structure representing the subset of C to which we want to compile.

Abstract C supports nested definitions of functions and static arrays. These are floated to the top level when transformed into C source code.
-}

module AbstractC where

import Control.Applicative
import Control.Monad
import "mtl" Control.Monad.State
import Data.List
import Data.Maybe
import Text.Printf

data Def = StaticArray { arrayName :: String, arrayType :: String, arrayElems :: [String] }
         | Function { functionName :: String, functionCode :: [Statement] }
         deriving (Show, Eq)

-- | Generate a C declaration for a Def.
genDecl :: Def -> String
genDecl (StaticArray name ty elems) = printf "static %s %s[%d];\n" ty name (length elems)
genDecl def@(Function name code) = unlines (nested ++ [top])
    where
        top = printf "StgFunPtr %s();" name
        nested =  map genDecl $ getNested def

-- | Generate C code for a Def.
genDef :: Def -> String
genDef (StaticArray name ty elems) = printf "static %s %s[] = {%s};\n" ty name (intercalate ", " $ map genElem elems)
    where
        genElem e = printf "(%s)%s" ty e
genDef def@(Function name code) = unlines (nested ++ [top])
    where
        top = printf "StgFunPtr %s() {\n%s}" name (concat $ catMaybes $ map genStatement code)
        nested = map genDef $ getNested def

-- | Represents a statement in a function body.
data Statement = NestedDef Def
               | Return String
               | Switch String [(String, [Statement])] (Maybe [Statement])
               | Code String
               deriving (Show, Eq)

-- | Generate C code for a function statement.
genStatement :: Statement -> Maybe String
genStatement (NestedDef _) = Nothing
genStatement (Return expr) = Just $ printf " return %s;\n" expr
genStatement (Switch base cases def) = Just $ printf " switch (%s) {\n%s }\n" base body
    where
        mkCase (value, code) = printf " case %s:\n%s break;\n" value $ concat $ catMaybes $ map genStatement code
        defaultCase Nothing = ""
        defaultCase (Just code) = printf " default:\n%s" $ concat $ catMaybes $ map genStatement code
        body = concat (map mkCase cases ++ [defaultCase def])
genStatement (Code x) = Just (' ':x ++ ";\n")

data GetNestedState = GNS [Def]

-- | Add a Def to the accumulator state.
addNested :: Def -> State GetNestedState ()
addNested def = do
    (GNS defs) <- get
    put (GNS (def:defs))

-- | Get a list of inner Defs.
getNested :: Def -> [Def]
getNested fun = let (GNS defs) = execState (mapM_ exNested $ functionCode fun) (GNS []) in defs

-- | Possibly extract a nested Def from s atatement.
exNested :: Statement -> State GetNestedState ()
exNested (NestedDef def) = addNested def
exNested _ = return ()
