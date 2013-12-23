{-# LANGUAGE TemplateHaskell, PackageImports, OverloadedStrings #-}

module Main where

import Bound
import Bound.Name hiding (name)
import Bound.Scope
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import "mtl" Control.Monad.Error
import "mtl" Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Char
import Data.Foldable as F
import Data.Int
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Traversable as T
import Data.Word
import qualified LLVM.General as FFI
import qualified LLVM.General.Context as FFI
import qualified LLVM.General.AST as LLVM
import qualified LLVM.General.AST.AddrSpace as LLVM
import qualified LLVM.General.AST.Attribute as LLVM
import qualified LLVM.General.AST.CallingConvention as LLVM
import qualified LLVM.General.AST.Constant as LLVM
import qualified LLVM.General.AST.DataLayout as LLVM
import qualified LLVM.General.AST.Float as LLVM
import qualified LLVM.General.AST.FloatingPointPredicate as LLVM
import qualified LLVM.General.AST.Global as LLVM
import qualified LLVM.General.AST.Global as LG
import qualified LLVM.General.AST.InlineAssembly as LLVM
import LLVM.General.AST.Instruction (Named((:=)))
import qualified LLVM.General.AST.Instruction as LLVM
import qualified LLVM.General.AST.Instruction as LLVMInst
import qualified LLVM.General.AST.IntegerPredicate as LLVM
import qualified LLVM.General.AST.Linkage as LLVM
import qualified LLVM.General.AST.Name as LLVM
import qualified LLVM.General.AST.Operand as LLVM
import qualified LLVM.General.AST.RMWOperation as LLVM
import qualified LLVM.General.AST.Type as LLVM
import qualified LLVM.General.AST.Visibility as LLVM
import qualified LLVM.General.DataLayout as LLVM
import qualified LLVM.General.PrettyPrint as LLVM
import Prelude.Extras
import System.Exit
import System.Process (system)
import Text.Printf
import qualified Unsafe.Coerce (unsafeCoerce)

infixl 9 :@
infixr 9 ~>

data Expr a = V a  -- Variable
            | L Int  -- Integer literal
            | Expr a :@ Expr a  -- Application
            | Lam (Scope (Name Ident ()) Expr a)  -- Lambda
            | Let [Scope (Name Ident Int) Expr a] (Scope (Name Ident Int) Expr a)  -- Recursive let binding
            | PrimFun PrimFun  -- Primitive function
            deriving (Eq,Ord,Show,Read)

data Type = TyFun Type Type
          | TyInt
          deriving (Eq,Ord,Show,Read)

(~>) :: Type -> Type -> Type
(~>) = TyFun

data Ident = Id { name :: String
                , typeOf :: Type
                } deriving (Eq,Ord,Show,Read)

data PrimFun = PrimBinOp PrimBinOp
             deriving (Eq,Ord,Show,Read)

data PrimBinOp = PrimAdd
               deriving (Eq,Ord,Show,Read)

instance Functor Expr where
    fmap = fmapDefault

instance Applicative Expr where
    pure = V
    (<*>) = ap

instance Foldable Expr where
    foldMap = foldMapDefault

instance Traversable Expr where
  traverse f (V a)    = V <$> f a
  traverse f (L l)    = pure $ L l
  traverse f (x :@ y)   = (:@) <$> traverse f x <*> traverse f y
  traverse f (Lam s)  = Lam <$> traverse f s
  traverse f (Let bs b) = Let <$> traverse (traverse f) bs <*> traverse f b

-- V :: a -> Expr a
-- (:@) :: Expr a -> Expr a -> Expr a
-- Lam :: (Scope () Expr a) -> Expr a

-- f(x) = x
-- λx.x
-- \x -> x
-- lam 'x' $ V 'x'

instance Eq1 Expr

instance Ord1 Expr

instance Show1 Expr

instance Read1 Expr

instance Monad Expr where
    return = V
    V a      >>= f = f a
    L l      >>= f = L l
    (x :@ y) >>= f = (x >>= f) :@ (y >>= f)
    Lam e    >>= f = Lam (e >>>= f)
    Let bs b >>= f = Let (map (>>>= f) bs) (b >>>= f)
    PrimFun pf >>= f = PrimFun pf

-- More like transitional definitions of 'Lam'.
lam :: Ident -> Expr Ident -> Expr Ident
lam v b = Lam (abstract1Name v b)

let_ :: [(Ident, Expr Ident)] -> Expr Ident -> Expr Ident
let_ [] b = b
let_ bs b = Let (map (abstr . snd) bs) (abstr b)
    where
        abstr = abstractName (`elemIndex` map fst bs)

whnf :: Expr a -> Expr a
whnf e@V{}   = e
whnf e@L{}   = e
whnf e@Lam{} = e
whnf e@PrimFun{} = e
whnf ((PrimFun (PrimBinOp pf)) :@ l :@ r) = case whnf l of
    L l' -> case whnf r of
        L r' -> whnf $ evalPrimBinOp pf l' r'
        otherwise -> error "Applying non prim value to prim fun"
    otherwise -> error "Applying non prim value to prim fun"
whnf (f :@ a) = case whnf f of
    Lam b -> whnf (instantiate1Name a b)
    f'    -> f' :@ a
whnf (Let bs b) = whnf (inst b)
    where
        es = map inst bs
        inst = instantiateName (es !!)

evalPrimBinOp :: PrimBinOp -> Int -> Int -> Expr a
evalPrimBinOp PrimAdd l r = L (l + r)

getFreeVars :: F.Foldable f => Scope b f a -> [a]
getFreeVars = foldMapScope (const mempty) (:[])

collectFreeVars :: Expr a -> [a]
collectFreeVars (Lam scope) = getFreeVars scope
collectFreeVars (Let _ scope) = getFreeVars scope
collectFreeVars _ = []

getArity :: Expr a -> Int
getArity (Lam scope) = 1 + (getArity $ fromScope scope)
getArity _ = 0

-- Module type.
data Module = Module { _modName :: String
                     , _tlDecls :: [(Ident, Expr Ident)]
                     } deriving (Show)

getTopLevelNames :: Module -> [String]
getTopLevelNames = nub . map (name . fst) . _tlDecls

makeLenses ''Module

testMain :: Expr Ident
testMain = lam id_in $ V id_add :@ L 5 :@ V id_in
    where
        id_in = Id "in" TyInt
        id_add = Id "add" (TyInt ~> TyInt ~> TyInt)

i = lam (Id "x" TyInt) $ V (Id "x" TyInt)
k = lam (Id "x" TyInt) $ lam (Id "y" TyInt) $ V (Id "x" TyInt)

testMod :: Module
testMod = Module "Test" [(id_id, i), (id_const, k), (id_main, testMain), (id_add, PrimFun $ PrimBinOp PrimAdd)]
    where
        id_id = Id "id" (TyInt ~> TyInt)
        id_add = Id "add" (TyInt ~> TyInt ~> TyInt)
        id_main = Id "main" (TyInt ~> TyInt)
        id_const = Id "const" (TyInt ~> TyInt ~> TyInt)

interpret :: Module -> Int -> Maybe Int
interpret (Module mn decls) input = do
    main <- lookup (Id "main" (TyInt ~> TyInt)) decls
    return $ eval $ whnf $ let_ decls $ main :@ L input

interpret_ :: Module -> Int -> Maybe (Expr Ident)
interpret_ (Module mn decls) input = do
    main <- lookup (Id "main" (TyInt ~> TyInt)) decls
    return $ whnf $ let_ decls $ main :@ L input

eval :: Expr a -> Int
eval (L r) = r
eval _ = error "Result is not an integer."

{-
Look for a decl called "main"
If "main" can't be found, error
Otherwise, make sure "main" is a function
If it isn't error
Otherwise, compile "main" as function recursively as follows:
    Collect all free variables in function
    For each free variable, look-up name in decl map
    If name found, link symbol
    Otherwise, look for function in decl set
    If found, compile function, store in decl map and link symbol
    Otherwise, error
-}

compileError :: MonadIO m => String -> m ()
compileError msg = liftIO $ do
    printf "Compilation error: %s\n" msg
    exitFailure

data CompilerState = CS { _compiled :: (M.Map String ()), _names :: [String], _decls :: [(Ident, Expr Ident)] }

makeLenses ''CompilerState

lookupTLD :: Ident -> CompilerState -> Maybe (Expr Ident)
lookupTLD ident (CS _ _ decls) = lookup ident decls

runErrorsOrQuit :: MonadIO m => ErrorT String m a -> m a
runErrorsOrQuit comp = do
    check <- runErrorT comp
    case check of
        Left err -> liftIO $ putStrLn err >> exitFailure
        Right x -> return x

systemWithErr :: String -> ErrorT String IO ()
systemWithErr command = do
    ret <- liftIO $ system command
    case ret of
        ExitFailure code -> throwError $ printf "Command \"%s\" failed returning exit code %d" command code
        ExitSuccess -> return ()

bracketSystem :: String -> String -> IO ()
bracketSystem command msg = do
    printf "%s... " msg
    runErrorsOrQuit . systemWithErr $ command
    putStrLn "Done."

compile :: Module -> IO ()
compile mod@(Module name decls) = do
    runErrorsOrQuit $ checkModule mod
    printf "Compiling module \"%s\"..." name
    out <- FFI.withContext $ \ctx -> do
        runErrorsOrQuit $ FFI.withModuleFromAST ctx (genModule (printf "%s.bc" name) mod :: LLVM.Module) $ \mod' -> do
            putStrLn =<< FFI.moduleString mod'
            runErrorsOrQuit $ FFI.writeBitcodeToFile (printf "%s.bc" name) mod'
    putStrLn " Done."
    bracketSystem (printf "llc %s.bc -o %s.as" name name) "Generating native ASM"
    bracketSystem (printf "gcc -o %sNative -xassembler %s.as" name name) "Building native target"

{-
Compile a function.

* Look for free variables in global environment. If one can't be matched, we have an error.
* For each free variable matched, compile it (if not already compiled) and link at relevant point.
* Create function type using LLVM.FFI.Core.functionTypeSource
* Add new function to module with LLVM.Wrapper.Core.addFunction
* Append basic blocks to function with LLVM.Wrapper.Core.appendBasicBlock
* Builder positionAtEnd
* Get parameters with getParam
* Build expression value
* Build return value
-}

maybeErr :: (Monad m, Error e) => e -> Maybe a -> ErrorT e m a
maybeErr msg comp = maybe (throwError msg) return comp

-- Super-simple sanity checking. Probably want to make this better at some point.
checkModule :: Module -> ErrorT String IO ()
checkModule (Module name decls) = do
    when (null name) $ throwError "Empty module name."
    let names = map fst decls
        diff = nub $ names \\ nub names
    unless (null diff) $ throwError $ printf "Redefined top-level bindings: %s." (show diff)
    mainF <- maybeErr "There must be a function `main` with type `Int -> Int`." $ lookup (Id "main" (TyInt ~> TyInt)) decls
    unless ((getArity $ whnf mainF) == 1) $ throwError "Main function must take 1 parameter only."

-- Internal function for displaying a runtime error message.
--runtimeError :: Function (Ptr Word8 -> VarArgs Word32) -> CodeGenModule (Function (Ptr Word8 -> IO ()))
--runtimeError printf = createFunction InternalLinkage $ \msg ->
--    withStringNul "Runtime error: %s\n" $ \t -> do
--        t' <- getElementPtr0 t (0 :: Word32, ())
--        let print = castVarArgs printf :: Function (Ptr Word8 -> Ptr Word8 -> IO Word32)
--        call print t' msg
--        ret ()

instance IsString LLVM.Name where
    fromString = LLVM.Name

stringConstant :: String -> LLVM.Constant
stringConstant = LLVM.Array (LLVM.IntegerType 8) . map mkCharConst
    where
        mkCharConst = LLVM.Int 8 . toInteger . ord

globalString :: String -> LLVM.Name -> LLVM.Definition
globalString str name = LLVM.GlobalDefinition (
                            LLVM.GlobalVariable name LLVM.Internal LLVM.Default Prelude.True (LLVM.AddrSpace 0) Prelude.True Prelude.True (LLVM.ArrayType (fromIntegral $ length str + 1) (LLVM.IntegerType 8)) (Just (stringConstant (str ++ "\00"))) Nothing 1)

genModule :: String -> Module -> LLVM.Module
genModule filepath mod = LLVM.Module filepath Nothing Nothing [
    globalString "Hello, world!" "msg"
    ,
    globalString "Error: No input provided" "errNoInput"
    ,
    globalString "%d\n" "inpFormat"
    ,
    LLVM.GlobalDefinition (
        LLVM.Function LLVM.External LLVM.Default LLVM.C [] (LLVM.IntegerType 32) "puts" ([LLVM.Parameter (LLVM.PointerType (LLVM.IntegerType 8) (LLVM.AddrSpace 0)) "str" []], False) [] Nothing 0 Nothing [])
    ,
    LLVM.GlobalDefinition (
        LLVM.Function LLVM.External LLVM.Default LLVM.C [] (LLVM.IntegerType 32) "atoi" ([LLVM.Parameter (LLVM.PointerType (LLVM.IntegerType 8) (LLVM.AddrSpace 0)) "str" []], False) [] Nothing 0 Nothing [])
    ,
    LLVM.GlobalDefinition (
        LLVM.Function LLVM.External LLVM.Default LLVM.C [] (LLVM.IntegerType 32) "printf" ([LLVM.Parameter (LLVM.PointerType (LLVM.IntegerType 8) (LLVM.AddrSpace 0)) "fmt" []], True) [] Nothing 0 Nothing [])
    ,
    LLVM.GlobalDefinition (genMainFunction mod)
    ]

genMainFunction :: Module -> LLVM.Global
genMainFunction mod = LLVM.Function { LG.linkage = LLVM.External
                                    , LG.visibility = LLVM.Default
                                    , LG.callingConvention = LLVM.C
                                    , LG.returnAttributes = []
                                    , LG.returnType = LLVM.IntegerType 32
                                    , LG.name = "main"
                                    , LG.parameters = (
                                        [ LLVM.Parameter (LLVM.IntegerType 32) "argc" []
                                        , LLVM.Parameter (LLVM.PointerType (LLVM.PointerType (LLVM.IntegerType 8) (LLVM.AddrSpace 0)) (LLVM.AddrSpace 0)) "argv" []
                                        ], False)
                                    , LG.functionAttributes = []
                                    , LG.section = Nothing
                                    , LG.alignment = 0
                                    , LG.garbageCollectorName = Nothing
                                    , LG.basicBlocks = [start, run, bErrNoInput]
                                    }
    where
        start = LLVM.BasicBlock "start" [ "cond" := LLVMInst.ICmp LLVM.SGT (LLVM.LocalReference "argc") (LLVM.ConstantOperand $ LLVM.Int 32 1) []
                                        ]
                                        (LLVM.Do (LLVM.CondBr (LLVM.LocalReference "cond") "run" "bErrNoInput" []))
        run = LLVM.BasicBlock "run" [ "tmp0" := LLVMInst.GetElementPtr Prelude.False (LLVM.LocalReference "argv") [LLVM.ConstantOperand (LLVM.Int 32 1)] []
                                    , "tmp1" := LLVMInst.Load Prelude.False (LLVM.LocalReference "tmp0") Nothing 1 []
                                    , "tmp2" := LLVMInst.GetElementPtr Prelude.False (LLVM.LocalReference "tmp1") [LLVM.ConstantOperand (LLVM.Int 32 0)] []
                                    , "inpV" := LLVM.Call Prelude.False LLVM.C [] (Right (LLVM.ConstantOperand $ LLVM.GlobalReference "atoi")) [(LLVM.LocalReference "tmp2", [])] [] []
                                    , "fmpTmp" := LLVMInst.GetElementPtr Prelude.False (LLVM.ConstantOperand $ LLVM.GlobalReference "inpFormat") [LLVM.ConstantOperand (LLVM.Int 32 0), LLVM.ConstantOperand (LLVM.Int 32 0)] []
                                    , LLVM.Do $ LLVM.Call Prelude.False LLVM.C [] (Right (LLVM.ConstantOperand $ LLVM.GlobalReference "printf")) [(LLVM.LocalReference "fmpTmp", []), (LLVM.LocalReference "inpV", [])] [] []
                                    ]
                                    (LLVM.Do (LLVM.Ret (Just (LLVM.ConstantOperand (LLVM.Int 32 0))) []))
        bErrNoInput = LLVM.BasicBlock "bErrNoInput" [ "tmp" := LLVMInst.GetElementPtr Prelude.False (LLVM.ConstantOperand $ LLVM.GlobalReference "errNoInput") [LLVM.ConstantOperand (LLVM.Int 32 0), LLVM.ConstantOperand (LLVM.Int 32 0)] []
                                                    , LLVM.Do $ LLVM.Call Prelude.False LLVM.C [] (Right (LLVM.ConstantOperand $ LLVM.GlobalReference "puts")) [(LLVM.LocalReference "tmp", [])] [] []
                                                    ]
                                                    (LLVM.Do (LLVM.Ret (Just (LLVM.ConstantOperand (LLVM.Int 32 1))) []))

compileTopLevelBinding :: (Ident, Expr Ident) -> LLVM.Definition
compileTopLevelBinding (ident, expr) = case whnf expr of
    Lam scope -> compileLambda (ident, expr)
    L x -> compileConstant (ident, expr)

compileLambda :: (Ident, Expr Ident) -> LLVM.Definition
compileLambda (Id name (TyFun TyInt r), Lam scope) = undefined
compileLambda (Id name (TyFun l r), Lam scope) = error "Higher-order functions are not currently supported."

genFunBase :: (Ident, Expr Ident) -> LLVM.Definition
genFunBase (ident, expr) = LLVM.Function { LLVM.linkage = LLVM.Internal
                                         , LLVM.visibility = LLVM.Default
                                         , LLVM.callingConvention = LLVM.ConstantOperand
                                         , LLVM.returnAttributes = []
                                         , LLVM.returnType = retType
                                         , LLVM.name = LLVM.Name $ name ident
                                         , LLVM.parameters = (params, Prelude.False)
                                         , LLVM.functionAttributes = []
                                         , LLVM.section = Nothing
                                         , LLVM.alignment = 0
                                         , LLVM.garbageCollectorName = Nothing
                                         , LLVM.basicBlocks = bbs
                                         }
    where
        (params, retType) = genFunParams $ typeOf ident
        bbs = undefined

compileConstant :: (Ident, Expr Ident) -> LLVM.Definition
compileConstant (Id name TyInt, L x) = LLVM.GlobalDefinition (LLVM.GlobalVariable (LLVM.Name name) LLVM.Internal LLVM.Default Prelude.True (LLVM.AddrSpace 0) Prelude.True Prelude.True (LLVM.IntegerType 32) (Just (LLVM.Int 32 $ toInteger x)) Nothing 0)
compileConstant (Id name ty, L _) = error $ printf "Type mismatch. Top-level constant `%s` has type `%s`. Expected `TyInt`." name (show ty)

compileLiteral :: Expr Ident -> LLVM.Constant
compileLiteral (L x) = LLVM.Int 32 $ toInteger x

compileFree :: Ident -> LLVM.Operand
compileFree (Id name _) = LLVM.ConstantOperand $ LLVM.GlobalReference $ LLVM.Name name

genFunParams :: Type -> ([LLVM.Parameter], LLVM.Type)
genFunParams ty = (mkParams (init tList) 1, last tList)
    where
        mkParams [] _ = []
        mkParams (t:ts) n = (LLVM.Parameter t (LLVM.UnName n) []) : mkParams ts (n+1)
        go TyInt = [LLVM.IntegerType 32]
        go (TyFun TyInt r) = LLVM.IntegerType 32 : go r
        go _ = error "Unsupported function type."
        tList = go ty

--compileApp :: Expr Ident -> [LLVM.Instruction]
--compileApp (l :@ r) = case typeOf l of
--    TyFun TyInt tyR -> case tyR of
--        TyInt -> 
--    otherwise -> error "Higher order functions not yet supported."

main :: IO ()
main = do
    let (Lam s) = testMain in print $ fromScope s
--main = compile testMod

-- Standard combinators.
--i = lam (Id "x" TyInt) $ V (Id "x" TyInt)
--k = lam (Id "x" TyInt) $ lam (Id "y" TyInt) $ V (Id "x" TyInt)
--s = lam "x" $ lam "y" $ lam "z" $ V "x" :@ V "z" :@ (V "y" :@ V "z")

--i' = lam "c" $ V "c"
--i'' = s :@ k :@ k

--t = lam "a" $ lam "b" $ V "a"
--f = lam "a" $ lam "b" $ V "b"

--and' = lam "m" $ lam "n" $ V "m" :@ V "n" :@ V "m"
--or' = lam "m" $ lam "n" $ V "m" :@ V "m" :@ V "n"
--not' = lam "m" $ lam "a" $ lam "b" $ V "m" :@ V "b" :@ V "a"
--if' = lam "m" $ lam "a" $ lam "b" $ V "m" :@ V "a" :@ V "b"

---- x  V "x"
---- f x  V "f" :@ V "x"
---- f (f x)  V "f" :@ V "f" :@ V "x"

--appn :: Int -> Expr a -> Expr a -> Expr a
--appn 0 x t = t
--appn n x t = x :@ appn (n-1) x t

--zero = lam "f" $ lam "x" $ V "x"
--one = lam "f" $ lam "x" $ V "f" :@ V "x"

--n i = lam "f" $ lam "x" $ appn i (V "f") (V "x")
