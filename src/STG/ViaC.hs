{-# LANGUAGE PackageImports #-}

module STG.ViaC where

import Control.Applicative
import Control.Monad
import "mtl" Control.Monad.State
import qualified Data.DList as D
import qualified Data.Map as M
import Data.Monoid
import Text.Printf

import AbstractC
import STG

-- | Generate a C program from an STG AST.
compile :: Program -> String
compile (Program bindings) = "#include \"RTS.h\"\n\n" ++ unlines (("//Decls." : map genDecl defs) ++ ("// Defs." : map genDef defs)) ++ "\nint main(int argc, char const *argv[]) { runRTS(main_closure, argc, argv); }\n"
    where
        defs = concatMap buildGlobal bindings

-- Helper for building info table defs in abstract C.
mkInfoTable :: String -> Def
mkInfoTable name = StaticArray (name ++ "_info") "StgWord" [name ++ "_entry"]

-- Helper for building static closure defs in abstract C.
mkStaticClosure :: String -> Def
mkStaticClosure name = StaticArray (name ++ "_closure") "StgWord" [name ++ "_info"]

-- Helper for building a static return vector.
mkReturnVector :: String -> Def
mkReturnVector name = StaticArray (name ++ "_retvec") "StgWord" [name ++ "_cont"]

-- Helper for building entry code function defs in abstract C.
mkEntryCode :: String -> [Statement] -> Def
mkEntryCode name = Function (name ++ "_entry")

-- Helper for building continuation function defs in abstract C.
mkContCode :: String -> [Statement] -> Def
mkContCode name = Function (name ++ "_cont")

-- | Generate abstract C for a top-level STG binding.
buildGlobal :: Binding -> [Def]
buildGlobal (Binding name lambda) = [infoTable, closure, entry]
    where
        infoTable = mkInfoTable name
        closure = mkStaticClosure name
        entry = mkEntryCode name $ buildEntry name lambda

type Unique = Int

-- | State type for accumulating statements and renamer mappings.
data Builder = Builder { code :: D.DList Statement, renameTable :: M.Map Var Var, nextUnique :: Unique }

-- | Append a new statement to the function body being built.
append :: Statement -> State Builder ()
append x = modify (\(Builder xs rt un) -> Builder (xs <> (D.fromList [x])) rt un)

-- | Add a rename mapping to the builder state.
renameVar :: Var -> Var -> State Builder ()
renameVar k v = modify (\(Builder xs rt un) -> Builder xs (M.insert k v rt) un)

-- | Check to see if a variable has been renamed.
getVar :: Var -> State Builder Var
getVar var = do
    (Builder _ table _) <- get
    pure $ M.findWithDefault var var table

-- | Generate a new unique name.
genUnique :: State Builder String
genUnique = do
    (Builder xs rt un) <- get
    put (Builder xs rt (un+1))
    pure ("u" ++ show un)

buildStatements :: State Builder a -> [Statement]
buildStatements comp = D.toList $ code $ execState comp $ Builder D.empty M.empty 0

-- | Generate abstract C for a lambda form's entry code.
buildEntry :: Var -> Lambda -> [Statement]
buildEntry self (Lambda free update args expr) = buildStatements go
    where
        go = do
            -- If the function is "main", we need to push a special continuation and return vector.
            when (self == "main") $ buildCont self $ do
                -- Escape interpreter.
                append $ Code "longjmp(jmpEnv, 1)"
            -- Collect free variables from closure.
            mapM_ buildFreeAssign $ zip free [1..]
            -- Collect args from stack.
            mapM_ buildArgAssign args
            -- Build function body.
            buildExpr self expr

-- | Generate abstract C for building a simple continuation return vector.
buildCont :: Var -> State Builder () -> State Builder ()
buildCont name code = do
    append $ NestedDef $ mkReturnVector name
    append $ NestedDef $ mkContCode name $ buildStatements code
    append $ Code $ printf "pushB((StgAddr)%s_retvec)" name

-- | Bind a free variable at an offset to a name.
buildFreeAssign :: (Var, Int) -> State Builder ()
buildFreeAssign (name, offset) = append $ Code $ printf "StgWord %s = node[%d]" name offset

-- | Pop an argument off the stack and bind it to a name.
buildArgAssign :: Var -> State Builder ()
buildArgAssign name = append $ Code $ printf "StgWord *%s = (StgWord*) popA()" name

-- | Generate abstract C code for an STG expression.
buildExpr :: Var -> Expr -> State Builder ()
buildExpr self (Let bindings expr) = do
    -- Make bindings.
    mapM_ (buildLetBinding self) bindings
    -- Build rest.
    buildExpr self expr
buildExpr self (Ap closure atoms) = do
    -- Push args onto stack.
    mapM_ (buildAtomPush self) atoms
    -- Enter closure.
    append $ Code $ printf "node = %s_closure" closure
    append $ Code "ENTER(node)"
buildExpr self (Prim prim atoms) = buildPrimCall self prim atoms

-- | Push an atom onto the argument stack.
buildAtomPush :: Var -> Atom -> State Builder ()
buildAtomPush self (AtomVar var) = do
    var' <- getVar var
    append $ Code $ printf "pushA((StgAddr) %s)" var'
--buildAtomPush self (AtomLit lit) = append $ Code $ printf "pushA((StgAddr) %d)" lit
buildAtomPush self (AtomLit lit) = do
    closure <- buildWrapLit self lit
    append $ Code $ printf "pushA((StgAddr) %s)" closure

-- | Build a new closure on the heap and bind it to a local name.
buildHeapClosure :: Var -> Var -> [Var] -> State Builder ()
buildHeapClosure parent name frees = do
    -- Start with pointer to info table.
    append $ Code $ printf "hp[0] = &%s_%s_info" parent name
    -- Then add free variables.
    forM_ (zip frees [1..]) $ \(free, offset) -> do
        append $ Code $ printf "hp[%d] = &%s" (offset::Int) free
    -- Name closure.
    append $ Code $ printf "StgWord *%s_%s_closure = (StgWord *) &hp[0]" parent name
    -- Increment heap pointer.
    append $ Code $ printf "hp += %d" (length frees + 1)

-- | Generate abstract C code for a let binding.
buildLetBinding :: Var -> Binding -> State Builder ()
buildLetBinding parent (Binding name lambda@(Lambda frees update args expr)) = do
    -- Build a new info table for this closure.
    let renamed = printf "%s_%s" parent name
    append $ NestedDef $ mkInfoTable renamed
    -- Build entry code.
    append $ NestedDef $ mkEntryCode renamed $ buildEntry renamed lambda
    -- Allocate the closure on the heap and make a local reference.
    buildHeapClosure parent name frees
    -- make a note in the builder state that this free variable has been renamed.
    renameVar name (renamed ++ "_closure")

-- | Pop continuation from return stack and jump to it.
buildContinuationPop :: State Builder ()
buildContinuationPop = append $ Code "JUMP(((StgAddr *)popB())[0])"

-- | Build a closure wrapping a literal.
buildWrapLit :: Var -> Literal -> State Builder Var
buildWrapLit self lit = do
    unique <- genUnique
    append $ Code "hp[0] = &_wrappedInt_info"
    append $ Code $ printf "hp[1] = (StgAddr)%d" lit
    let closureName = printf "%s_%s_closure" self unique
    append $ Code $ printf "StgWord *%s = (StgWord *) &hp[0]" closureName
    append $ Code "hp += 2"
    pure $ closureName

buildEvalAtomPush :: Var -> Atom -> State Builder () -> State Builder ()
buildEvalAtomPush self (AtomLit x) bcont = do
    -- Generate a new wrapper closure for this literal.
    closure <- buildWrapLit self x
    buildCont closure bcont
    -- Enter closure.
    append $ Code $ printf "node = %s" closure
    append $ Code "ENTER(node)"
buildEvalAtomPush self (AtomVar closure) bcont = do
    -- Make a continuation to run after eval.
    let contName = printf "%s_%s" self closure
    buildCont contName bcont
    -- Enter closure.
    closureVar <- getVar closure
    append $ Code $ printf "node = %s" closureVar
    append $ Code "ENTER(node)"

-- | Build abstract C code for calling a primitive function provided by the RTS.
buildPrimCall :: Var -> Prim -> [Atom] -> State Builder ()
buildPrimCall self prim [AtomLit x, AtomLit y] = do
    buildAtomPush self $ AtomLit x
    buildAtomPush self $ AtomLit y
    append $ Code $ printf "JUMP(%s)" $ primMap prim
buildPrimCall self prim [a1, a2@(AtomLit y)] = do
    buildEvalAtomPush self a1 $ do
        buildArgAssign "tmp"
        append $ Code "pushA((StgAddr) retInt)"
        buildAtomPush self a2
        append $ Code $ printf "JUMP(%s)" $ primMap prim
buildPrimCall self prim [a1, a2@(AtomVar a2var)] = do
    buildAtomPush self a2
    buildEvalAtomPush self a1 $ do
        buildArgAssign a2var
        append $ Code "pushA((StgAddr) retInt)"
        buildEvalAtomPush self a2 $ do
            append $ Code "pushA((StgAddr) retInt)"
            append $ Code $ printf "JUMP(%s)" $ primMap prim

primMap :: Prim -> Var
primMap PrimAdd = "_primIntAdd_entry"
primMap PrimMul = "_primIntMul_entry"
