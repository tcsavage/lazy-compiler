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
        defs = concatMap (buildGlobal (makeGlobalMap bindings)) bindings

makeGlobalMap :: [Binding] -> M.Map Var Var
makeGlobalMap = M.fromList . (++ builtIns) . map (\(Binding var _) -> (var, var ++ "_closure"))
    where
        builtIns = [("dumpInt", "dumpInt_closure")]

-- Helper for building info table defs in abstract C.
mkInfoTable :: String -> Def
mkInfoTable name = StaticArray (name ++ "_info") "StgWord" [name ++ "_entry"]

-- Helper for building static closure defs in abstract C.
mkStaticClosure :: String -> Def
mkStaticClosure name = StaticArray (name ++ "_closure") "StgWord" [name ++ "_info"]

-- Helper for building a static return vector.
mkReturnVector :: String -> [String] -> Def
mkReturnVector name branches = StaticArray (name ++ "_retvec") "StgWord" $ map (++ "_cont") branches

-- Helper for building a static simple return vector.
mkSimpleReturnVector :: String -> Def
mkSimpleReturnVector name = StaticArray (name ++ "_retvec") "StgWord" [name ++ "_cont"]

-- Helper for building entry code function defs in abstract C.
mkEntryCode :: String -> [Statement] -> Def
mkEntryCode name = Function (name ++ "_entry")

-- Helper for building continuation function defs in abstract C.
mkContCode :: String -> [Statement] -> Def
mkContCode name = Function (name ++ "_cont")

-- | Generate abstract C for a top-level STG binding.
buildGlobal :: M.Map Var Var -> Binding -> [Def]
buildGlobal globalMap (Binding name lambda) = [infoTable, closure, entry]
    where
        infoTable = mkInfoTable name
        closure = mkStaticClosure name
        entry = mkEntryCode name $ buildEntry globalMap name lambda

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

buildStatements :: State Builder a -> State Builder [Statement]
buildStatements comp = do
    rt <- gets renameTable
    pure $ D.toList $ code $ execState comp $ Builder D.empty rt 0

-- | Generate abstract C for a lambda form's entry code.
buildEntry :: M.Map Var Var -> Var -> Lambda -> [Statement]
buildEntry globalMap self (Lambda free update args expr) = D.toList $ code $ execState go $ Builder D.empty globalMap 0
    where
        go = do
            -- If the function is "main", we need to push a special continuation and return vector.
            when (self == "main") $ buildCont "_root" $ do
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
    append $ NestedDef $ mkSimpleReturnVector name
    statements <- buildStatements code
    append $ NestedDef $ mkContCode name statements
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
buildExpr self (Case expr alts) = do
    -- Build the return vector from the alternatives (and push it onto the stack).
    buildCaseVectoredReturn self alts
    -- Evaluate expression.
    buildExpr self expr
buildExpr self (Ap closure atoms) = do
    -- Push args onto stack.
    mapM_ (buildAtomPush self) atoms
    -- Enter closure.
    closure' <- getVar closure
    append $ Code $ printf "node = %s" closure'
    append $ Code "ENTER(node)"
buildExpr self (Constr tag atoms) = do
    buildConstructorClosure self tag atoms
buildExpr self (Prim prim atoms) = buildPrimCall self prim atoms

-- | Push an atom onto the argument stack.
buildAtomPush :: Var -> Atom -> State Builder ()
buildAtomPush self (AtomVar var) = do
    var' <- getVar var
    append $ Code $ printf "pushA((StgAddr) %s)" var'
--buildAtomPush self (AtomLit lit) = append $ Code $ printf "pushA((StgAddr) %d)" lit
buildAtomPush self atom@(AtomLit lit) = do
    closure <- buildWrapLit self atom
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

buildConstructorClosure :: Var -> Constr -> [Atom] -> State Builder ()
buildConstructorClosure self tag atoms = do
    unique <- genUnique
    -- Set info table.
    append $ Code $ printf "hp[0] = &_constructor%d_info" $ length atoms
    -- Set tag.
    append $ Code $ printf "hp[1] = %d" tag
    -- Set data.
    forM_ (zip atoms [(2::Int)..]) $ \(atom, offset) -> case atom of
        (AtomVar var) -> append $ Code $ printf "hp[%d] = %s" offset var
        (AtomLit lit) -> do
            litClosure <- buildWrapLit self (AtomLit lit)
            append $ Code $ printf "hp[%d] = %s" offset litClosure
    -- Name closure.
    append $ Code $ printf "StgWord *%s_cons%d_%s_closure = (StgWord *) &hp[0]" self tag unique
    -- Increment heap pointer.
    append $ Code $ printf "hp += %d" (length atoms + 2)

-- | Generate abstract C code for a let binding.
buildLetBinding :: Var -> Binding -> State Builder ()
buildLetBinding parent (Binding name lambda@(Lambda frees update args expr)) = do
    -- Build a new info table for this closure.
    let renamed = printf "%s_%s" parent name
    append $ NestedDef $ mkInfoTable renamed
    -- Build entry code.
    rt <- gets renameTable
    append $ NestedDef $ mkEntryCode renamed $ buildEntry rt renamed lambda
    -- Allocate the closure on the heap and make a local reference.
    buildHeapClosure parent name frees
    -- make a note in the builder state that this free variable has been renamed.
    renameVar name (renamed ++ "_closure")

-- | Pop continuation from return stack and jump to it.
buildContinuationPop :: State Builder ()
buildContinuationPop = append $ Code "JUMP(((StgAddr *)popB())[0])"

-- | Build a closure wrapping a literal.
buildWrapLit :: Var -> Atom -> State Builder Var
buildWrapLit self atom = do
    unique <- genUnique
    append $ Code "hp[0] = &_wrappedInt_info"
    let val = case atom of AtomVar var -> var
                           AtomLit lit -> show lit
    append $ Code $ printf "hp[1] = (StgAddr)%s" val
    let closureName = printf "%s_%s_closure" self unique
    append $ Code $ printf "StgWord *%s = (StgWord *) &hp[0]" closureName
    append $ Code "hp += 2"
    pure $ closureName

buildEvalAtomPush :: Var -> Atom -> State Builder () -> State Builder ()
buildEvalAtomPush self atom@(AtomLit x) bcont = do
    -- Generate a new wrapper closure for this literal.
    closure <- buildWrapLit self atom
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

buildCaseVectoredReturn :: Var -> Alts -> State Builder ()
buildCaseVectoredReturn self (Algebraic aalts def) = do
    -- For each algebraic alt...
    names <- forM (zip aalts [(0::Int)..]) $ \(AAlt tag bindings expr, n) -> do
        let name = printf "%s_alt%d" self n
        -- Create the continuation.
        statements <- buildStatements $ do
            forM_ (zip bindings [(2::Int)..]) $ \(binder, offset) -> do
                append $ Code $ printf "StgWord *%s = node[%d]" binder offset
            buildExpr self expr 
        append $ NestedDef $ mkContCode name statements
        pure name
    -- Build default alternative.
    statements <- buildStatements $ do
        buildDefaultAAlt self def
    let defAltName = self ++ "_altDEF"
    append $ NestedDef $ mkContCode defAltName statements
    -- Create and push return vector.
    append $ NestedDef $ mkReturnVector self (names ++ [defAltName])
    append $ Code $ printf "pushB((StgAddr)%s_retvec)" self
    -- Create and push switch return vector.
    buildCont (self ++ "_switch") $ do
        cases <- mapM buildAlgSwitchCase (zip aalts [0..])
        append $ Switch "rTag" cases (Just [Code $ printf "JUMP(((StgAddr *)popB())[%d])" $ length aalts])
buildCaseVectoredReturn self (Primitive palts def) = do
    -- For each primitive alt...
    names <- forM (zip palts [(0::Int)..]) $ \(PAlt lit expr, n) -> do
        let name = printf "%s_alt%d" self n
        -- Create the continuation.
        statements <- buildStatements $ do
            buildExpr self expr
        append $ NestedDef $ mkContCode name statements
        pure name
    -- Build default alternative.
    statements <- buildStatements $ do
        buildDefaultPAlt self def
    let defAltName = self ++ "_altDEF"
    append $ NestedDef $ mkContCode defAltName statements
    -- Create and push return vector.
    append $ NestedDef $ mkReturnVector self (names ++ [defAltName])
    append $ Code $ printf "pushB((StgAddr)%s_retvec)" self
    -- Create and push switch return vector.
    buildCont (self ++ "_switch") $ do
        cases <- mapM buildPrimSwitchCase (zip palts [0..])
        append $ Switch "retInt" cases (Just [Code $ printf "JUMP(((StgAddr *)popB())[%d])" $ length palts])

buildPrimSwitchCase :: (PAlt, Int) -> State Builder (String, [Statement])
buildPrimSwitchCase (PAlt lit _, n) = do
    code <- buildStatements $ do
        append $ Code $ printf "JUMP(((StgAddr *)popB())[%d])" n
    pure (show lit, code)

buildAlgSwitchCase :: (AAlt, Int) -> State Builder (String, [Statement])
buildAlgSwitchCase (AAlt tag _ _, n) = do
    code <- buildStatements $ do
        append $ Code $ printf "JUMP(((StgAddr *)popB())[%d])" n
    pure (show tag, code)

-- | Build code for default primitive alternative.
buildDefaultPAlt :: Var -> DefAlt -> State Builder ()
buildDefaultPAlt self (DefBinding name expr) = do
    append $ Code $ printf "StgInt %s = retInt" name
    closure <- buildWrapLit self (AtomVar name)
    renameVar name closure
    buildExpr self expr
buildDefaultPAlt self (Default expr) = buildExpr self expr

-- | Build code for default algebraic alternative.
buildDefaultAAlt :: Var -> DefAlt -> State Builder ()
buildDefaultAAlt self (DefBinding name expr) = do
    append $ Code $ printf "StgWord *%s = node" name
    buildExpr self expr
buildDefaultAAlt self (Default expr) = buildExpr self expr
