{-# LANGUAGE PackageImports #-}

module STG.ViaC (compile) where

import Control.Applicative
import Control.Monad
import "mtl" Control.Monad.State
import qualified Data.DList as D
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Text.Printf

import AbstractC
import STG.AST
import STG.ViaC.Builder

-- | Generate a C program from an STG AST.
compile :: Program -> String
compile (Program bindings) = "#include \"RTS.h\"\n\n" ++ unlines (("// Decls." : map genDecl defs) ++ ("// Defs." : map genDef defs)) ++ "\nint main(int argc, char const *argv[]) { runRTS(main_closure, argc, argv); }\n"
    where
        defs = concatMap (buildGlobal (makeGlobalMap bindings)) bindings

-- | Build a map of variables from a list of top-level bindings,
makeGlobalMap :: [Binding] -> M.Map Var CVar
makeGlobalMap = M.fromList . (++ builtIns) . map (\(Binding var _) -> (var, Global var (var ++ "_closure")))
    where
        -- Register dumpInt as a built-in.
        builtIns = [("dumpInt", Global "dumpInt" "dumpInt_closure")]

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
buildGlobal :: M.Map Var CVar -> Binding -> [Def]
buildGlobal globalMap (Binding name lambda) = [infoTable, closure, entry]
    where
        infoTable = mkInfoTable name
        closure = mkStaticClosure name
        entry = mkEntryCode name $ buildEntry globalMap name lambda

-- | Generate abstract C for a lambda form's entry code.
buildEntry :: M.Map Var CVar -> Var -> Lambda -> [Statement]
buildEntry globals self (Lambda free update args expr) = execBuilder [self] env args $ do
    -- If the function is "main", we need to push a special continuation and return vector.
    when (self == "main") $ buildCont "_root" $ do
        -- Escape interpreter.
        append $ Code "longjmp(jmpEnv, 1)"
    -- Pop args from stack.
    mapM_ popVar args
    -- Build function body.
    buildExpr self expr
  where
    -- Build free variable mapping.
    freeEnv = M.fromList $ map (\(name, offset) -> (name, Node name offset)) (zip free [0..])
    -- Build arg mapping.
    argEnv = M.fromList $ map (\name -> (name, Stack name)) args
    -- Create initial environment from globals, free variables and arguments.
    env = globals `M.union` (freeEnv `M.union` argEnv)

-- | Generate abstract C code for an STG expression.
buildExpr :: Var -> Expr -> BuilderM ()
buildExpr self (Let bindings expr) = do
    -- Make bindings.
    mapM_ (buildLetBinding self) bindings
    -- Build rest.
    buildExpr self expr
buildExpr self (LetRec bindings expr) = do
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
    closure' <- getVarRef closure
    append $ Code $ printf "node = %s" closure'
    append $ Code "ENTER(node)"
buildExpr self (Constr tag atoms) = do
    closure <- buildConstructorClosure self tag atoms
    --pushVar closure
    closure' <- getVarRef closure
    append $ Code $ printf "node = %s" closure'
    append $ Code $ printf "rTag = %d" tag
    append $ Code "JUMP(((StgAddr *)popB())[0])"
buildExpr self (Prim prim atoms) = buildPrimCall self prim atoms
buildExpr self (Literal lit) = do
    append $ Code $ printf "retInt = %d" lit
    append $ Code "JUMP(((StgAddr *)popB())[0])"

-- | Generate abstract C code for a let binding.
buildLetBinding :: Var -> Binding -> BuilderM ()
buildLetBinding self (Binding name lambda@(Lambda frees update args expr)) = withAddedPrefix name $ do
    -- Build a new info table for this closure.
    renamed <- getPrefix
    append $ NestedDef $ mkInfoTable renamed
    -- Build entry code.
    env <- getEnv
    let env' = foldr (\(free, offset) e -> M.insert free (Node free offset) e) env $ zip frees [0..]
    append $ NestedDef $ mkEntryCode renamed $ buildEntry env' renamed lambda
    -- Allocate the closure on the heap and make a local reference.
    buildHeapClosure self name frees
    -- Save environment for use in the let binding.
    pure ()
    --saveEnvironment

buildCaseVectoredReturn :: Var -> Alts -> BuilderM ()
buildCaseVectoredReturn self (Algebraic aalts def) = do
    -- For each algebraic alt...
    names <- forM (zip aalts [(0::Int)..]) $ \(AAlt tag bindings expr, n) -> withAddedPrefix (printf "alt%d" n) $ do
        name <- getPrefix
        -- Create the continuation.
        statements <- buildStatements $ do
            forM_ (zip bindings [(2::Int)..]) $ \(binder, offset) -> do
                append $ Code $ printf "StgWord *%s = (StgWord *) node[%d]" binder offset
            buildExpr self expr 
        append $ NestedDef $ mkContCode name statements
        pure name
    -- Build default alternative.
    statements <- buildStatements $ do
        buildDefaultAAlt self def
    withAddedPrefix "altDEF" $ do
        defAltName <- getPrefix
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
    names <- forM (zip palts [(0::Int)..]) $ \(PAlt lit expr, n) -> withAddedPrefix (printf "alt%d" n) $ do
        name <- getPrefix
        -- Create the continuation.
        statements <- buildStatements $ do
            buildExpr self expr
        append $ NestedDef $ mkContCode name statements
        pure name
    -- Build default alternative.
    statements <- buildStatements $ do
        buildDefaultPAlt self def
    withAddedPrefix "altDEF" $ do
        defAltName <- getPrefix
        append $ NestedDef $ mkContCode defAltName statements
        -- Create and push return vector.
        append $ NestedDef $ mkReturnVector self (names ++ [defAltName])
        append $ Code $ printf "pushB((StgAddr)%s_retvec)" self
        -- Create and push switch return vector.
        buildCont (self ++ "_switch") $ do
            cases <- mapM buildPrimSwitchCase (zip palts [0..])
            append $ Switch "retInt" cases (Just [Code $ printf "JUMP(((StgAddr *)popB())[%d])" $ length palts])

buildAlgSwitchCase :: (AAlt, Int) -> BuilderM (String, [Statement])
buildAlgSwitchCase (AAlt tag _ _, n) = do
    code <- buildStatements $ do
        -- Pop return vector and jump to continuation for this alt.
        append $ Code $ printf "JUMP(((StgAddr *)popB())[%d])" n
    pure (show tag, code)

buildPrimSwitchCase :: (PAlt, Int) -> BuilderM (String, [Statement])
buildPrimSwitchCase (PAlt lit _, n) = do
    code <- buildStatements $ do
        -- Pop return vector and jump to continuation for this alt.
        append $ Code $ printf "JUMP(((StgAddr *)popB())[%d])" n
    pure (show lit, code)

-- | Build code for default algebraic alternative.
buildDefaultAAlt :: Var -> DefAlt -> BuilderM ()
buildDefaultAAlt self (DefBinding name expr) = do
    localNode name
    buildExpr self expr
buildDefaultAAlt self (Default expr) = buildExpr self expr

-- | Build code for default primitive alternative.
buildDefaultPAlt :: Var -> DefAlt -> BuilderM ()
buildDefaultPAlt self (DefBinding name expr) = do
    buildBoxedReg name IntReturn
    buildExpr self expr
buildDefaultPAlt self (Default expr) = buildExpr self expr

buildConstructorClosure :: Var -> Constr -> [Atom] -> BuilderM Var
buildConstructorClosure self tag atoms = do
    unique <- genUnique
    -- Set info table.
    append $ Code $ printf "hp[0] = &_constructor%d_info" $ length atoms
    -- Set tag.
    append $ Code $ printf "hp[1] = (StgAddr)%d" tag
    -- Set data.
    forM_ (zip atoms [(2::Int)..]) $ \(atom, offset) -> case atom of
        (AtomVar var) -> do
            cname <- getVarRef var
            append $ Code $ printf "hp[%d] = (StgAddr) %s" offset cname
        (AtomLit lit) -> do
            litClosure <- buildBoxedLit self lit
            append $ Code $ printf "hp[%d] = (StgAddr) %s" offset litClosure
    -- Name closure.
    let cname = printf "%s_cons%d_closure" unique tag
    append $ Code $ printf "StgWord *%s = (StgWord *) &hp[0]" cname
    newLocal cname cname
    -- Increment heap pointer.
    append $ Code $ printf "hp += %d" (length atoms + 2)
    -- Return name.
    pure cname

-- | Build abstract C code for calling a primitive function provided by the RTS.
buildPrimCall :: Var -> Prim -> [Atom] -> BuilderM ()
buildPrimCall self prim [AtomLit x, AtomLit y] = do
    -- When we have two literal atoms, push both and jump straight to the primop code.
    buildAtomPush self $ AtomLit x
    buildAtomPush self $ AtomLit y
    append $ Code $ printf "JUMP(%s)" $ primMap prim
buildPrimCall self prim [a1, a2@(AtomLit y)] = do
    -- When first arg is var atom and second is literal, eval and push first, push second, and jump to primop.
    buildEvalAtomPush self a1 $ do
        tempName <- ("temp_"++) <$> genUnique
        pushReg tempName IntReturn
        buildAtomPush self a2
        append $ Code $ printf "JUMP(%s)" $ primMap prim
buildPrimCall self prim [a1, a2@(AtomVar a2var)] = do
    buildAtomPush self a2
    buildEvalAtomPush self a1 $ do
        popVar a2var
        tempName <- ("temp_"++) <$> genUnique
        pushReg tempName IntReturn
        buildEvalAtomPush self a2 $ do
            tempName2 <- ("temp_"++) <$> genUnique
            pushReg tempName2 IntReturn
            append $ Code $ printf "JUMP(%s)" $ primMap prim

-- | Simple mapping of primops to entry code names.
primMap :: Prim -> Var
primMap PrimAdd = "_primIntAdd_entry"
primMap PrimMul = "_primIntMul_entry"

-- | Generate abstract C for building a simple continuation return vector.
buildCont :: Var -> BuilderM () -> BuilderM ()
buildCont name code = do
    append $ NestedDef $ mkSimpleReturnVector name
    statements <- buildStatements code
    append $ NestedDef $ mkContCode name statements
    append $ Code $ printf "pushB((StgAddr)%s_retvec)" name

-- | Push an atom onto the argument stack.
buildAtomPush :: Var -> Atom -> BuilderM ()
buildAtomPush self (AtomVar var) = pushVar var
buildAtomPush self (AtomLit lit) = do
    closure <- buildBoxedLit self lit
    pushVar closure

-- | Evaluate atom and push result.
buildEvalAtomPush :: Var -> Atom -> BuilderM () -> BuilderM ()
buildEvalAtomPush self (AtomLit x) bcont = do
    -- Generate a new wrapper closure for this literal.
    closure <- buildBoxedLit self x
    buildCont closure bcont
    -- Enter closure.
    append $ Code $ printf "node = %s" closure
    append $ Code "ENTER(node)"
buildEvalAtomPush self (AtomVar closure) bcont = do
    -- Make a continuation to run after eval.
    let contName = printf "%s_%s" self closure
    buildCont contName bcont
    -- Enter closure.
    closureVar <- getVarRef closure
    append $ Code $ printf "node = %s" closureVar
    append $ Code "ENTER(node)"

-- | Build a closure wrapping a literal.
buildBoxedLit :: Var -> Literal -> BuilderM Var
buildBoxedLit self lit = do
    unique <- genUnique
    append $ Code "hp[0] = &_wrappedInt_info"
    append $ Code $ printf "hp[1] = (StgAddr)%s" (show lit)
    let closureName = printf "%s_closure" unique
    append $ Code $ printf "StgWord *%s = (StgWord *) &hp[0]" closureName
    append $ Code "hp += 2"
    newLocal closureName closureName
    pure closureName

-- | Build a closure wrapping the value of a register.
buildBoxedReg :: Var -> Register -> BuilderM CVar
buildBoxedReg name reg = do
    unique <- genUnique
    append $ Code "hp[0] = &_wrappedInt_info"
    append $ Code $ printf "hp[1] = (StgAddr) %s" (regRef reg)
    let closureName = printf "%s_closure" unique
    append $ Code $ printf "StgWord *%s = (StgWord *) &hp[0]" closureName
    append $ Code "hp += 2"
    newLocal name closureName

-- | Build a new closure on the heap and bind it to a local name.
buildHeapClosure :: Var -> Var -> [Var] -> BuilderM CVar
buildHeapClosure self name frees = do
    prefix <- getPrefix
    -- Start with pointer to info table.
    append $ Code $ printf "hp[0] = &%s_info" prefix
    -- Then add free variables.
    forM_ (zip frees [1..]) $ \(free, offset) -> do
        freeRef <- getVarRef free
        append $ Code $ printf "hp[%d] = %s" (offset::Int) freeRef
    -- Name closure.
    let localname = printf "%s_closure" prefix
    append $ Code $ printf "StgWord *%s = (StgWord *) &hp[0]" localname
    -- Increment heap pointer.
    append $ Code $ printf "hp += %d" (length frees + 1)
    -- Update the builder state and return the CVar in case it's wanted.
    newLocal name localname
