{-# LANGUAGE PackageImports #-}

module STG.ViaC.Builder
( CVar(..)
, Register(..)
, BuilderM
, append
, genUnique
, getPrefix
, withAddedPrefix
, getVar
, varRef
, getVarRef
, pushVar
, popVar
, newLocal
, regRef
, pushReg
, localReg
, localNode
, saveEnvironment
, getEnv
, execBuilder
, buildStatements
) where

import Control.Applicative
import Control.Monad
import "mtl" Control.Monad.State
import qualified Data.DList as D
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Text.Printf

import AbstractC
import STG.AST

data CVar = Stack Var
          | Node Var Int
          | Global Var Var
          | Local Var Var
          deriving (Show, Eq)

data Register = IntReturn | TagReturn deriving (Show, Eq)

type Unique = Int

-- | State type for accumulating statements and renamer mappings.
data Builder = Builder { prefix :: [String], code :: D.DList Statement, renameTable :: M.Map Var CVar, aStack :: [Var], nextUnique :: Unique }

type BuilderM a = State Builder a

-- | Append a new statement to the function body being built.
append :: Statement -> BuilderM ()
append x = do
    xs <- gets code
    modify (\builder -> builder { code = xs <> (D.fromList [x]) })

-- | Generate a new unique name.
genUnique :: BuilderM String
genUnique = do
    pfix <- intercalate "_" <$> gets prefix
    un <- gets nextUnique
    modify (\builder -> builder { nextUnique = un+1 })
    pure (pfix ++ "_u" ++ show un)

getPrefix :: BuilderM String
getPrefix = intercalate "_" <$> gets prefix

-- | Add a prefix section for the action and remove it afterwards.
withAddedPrefix :: String -> BuilderM a -> BuilderM a
withAddedPrefix pfix comp = do
    pstate <- gets prefix
    modify (\builder -> builder { prefix = pfix:pstate })
    r <- comp
    modify (\builder -> builder { prefix = pstate })
    pure r

getVar :: Var -> BuilderM CVar
getVar var = do
    mapping <- gets renameTable
    pure $ M.findWithDefault (Local var var) var mapping

varRef :: CVar -> BuilderM String
varRef (Stack var) = do
    stack <- gets aStack
    let i = fromJust $ elemIndex var stack
    pure $ printf "spA[-%d]" (i+1)
varRef (Node name offset) = pure $ printf "node[%d]" (offset+1)
varRef (Global name cname) = pure cname
varRef (Local name cname) = pure cname

getVarRef :: Var -> BuilderM String
getVarRef = varRef <=< getVar

-- Add a var reference to the stack state.
pushStack :: Var -> BuilderM ()
pushStack var = do
    stack <- gets aStack
    modify (\builder -> builder { aStack = var:stack })

-- Pop a var reference from the stack state.
popStack :: BuilderM Var
popStack = do
    stack <- gets aStack
    case stack of
        [] -> do env <- getEnv
                 pfix <- intercalate "_" <$> gets prefix
                 error $ printf "Encountered empty stack state when trying to pop. Current env:\n%s\nCurrent prefix: %s" (show env) pfix
        (x:rest) -> do modify (\builder -> builder { aStack = rest })
                       pure x

-- Update the variable state.
updateVar :: Var -> CVar -> BuilderM ()
updateVar var new = do
    mapping <- gets renameTable
    modify (\builder -> builder { renameTable = M.insert var new mapping })

-- | Push a variable onto the stack. If the variable is already on the stack, this does nothing.
pushVar :: Var -> BuilderM ()
pushVar var = do
    cvar <- getVar var  -- Get variable state in local environment.
    case cvar of
        Stack _ -> pure ()  -- Do nothing if already on the stack.
        _ -> do pushStack $ getName cvar  -- Add name to stack state.
                ref <- varRef cvar  -- Get C-level ref.
                append $ Code $ printf "spA[0] = (StgAddr)%s" ref  -- Assign ref to stack head.
                append $ Code "++spA"  -- Update stack pointer.
                updateVar var (Stack var)  -- Tell the state that this variable is now located on the stack.
  where
    getName (Node name _) = name
    getName (Local name cname) = name
    getName (Global name cname) = name

popVar :: Var -> BuilderM CVar
popVar name = do
    cname <- genUnique
    append $ Code $ printf "StgWord *%s = (StgWord*) popA()" cname
    stackName <- popStack
    let cvar = Local name cname
    updateVar name cvar
    updateVar stackName cvar
    pure cvar

newLocal :: Var -> Var -> BuilderM CVar
newLocal name cname = do
    let cvar = Local name cname
    mapping <- gets renameTable
    modify (\builder -> builder { renameTable = M.insert name cvar mapping })
    pure cvar

regRef :: Register -> String
regRef IntReturn = "retInt"
regRef TagReturn = "rTag"

pushReg :: Var -> Register -> BuilderM ()
pushReg name reg = do
    append $ Code $ printf "spA[0] = (StgAddr)%s" $ regRef reg  -- Assign register to stack head.
    append $ Code "++spA"  -- Update stack pointer.
    pushStack name
    updateVar name (Stack name)  -- Tell the state that this variable is now located on the stack.

localReg :: Var -> Register -> BuilderM CVar
localReg name reg = do
    let cvar = Local name name
    append $ Code $ printf "StgWord *%s = (StgWord *) %s" name (regRef reg)
    updateVar name cvar  -- Tell the state that this variable is now located on the stack.
    pure cvar

localNode :: Var -> BuilderM CVar
localNode name = do
    unique <- genUnique
    let cvar = Local name unique
    append $ Code $ printf "StgWord *%s = (StgWord *) node" unique
    updateVar name cvar
    pure cvar

-- | Copy any local or node variable onto the stack.
saveEnvironment :: BuilderM ()
saveEnvironment = do
    mapping <- M.toList <$> getEnv
    forM_ mapping $ \(name, cvar) -> do
        case cvar of
            Local name cname -> pushVar name
            Node name offset -> pushVar name
            _ -> pure ()

getEnv :: BuilderM (M.Map Var CVar)
getEnv = gets renameTable

-- | Generate abstract C statements.
execBuilder :: [String] -> M.Map Var CVar -> [Var] -> BuilderM a -> [Statement]
execBuilder pstate globals initialStack comp = D.toList $ code $ execState comp $ Builder pstate D.empty globals initialStack 0

buildStatements :: BuilderM a -> BuilderM [Statement]
buildStatements comp = do
    pstate <- gets prefix
    rt <- gets renameTable
    sa <- gets aStack
    pure $ execBuilder pstate rt sa comp
