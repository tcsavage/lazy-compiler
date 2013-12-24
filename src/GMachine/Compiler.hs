{-# LANGUAGE TemplateHaskell, PackageImports #-}

module GMachine.Compiler where

import Control.Applicative
import Control.Lens
import Control.Monad
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Writer
--import qualified LLVM.General as FFI
--import qualified LLVM.General.Context as FFI
import qualified LLVM.General.AST as LLVM
import qualified LLVM.General.AST.AddrSpace as LLVM
import qualified LLVM.General.AST.Attribute as LLVM
import qualified LLVM.General.AST.CallingConvention as LLVM
import qualified LLVM.General.AST.Constant as LLVM
import qualified LLVM.General.AST.Constant as LC
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

import GMachine

data CodeGenState = CGS { _globalIdNext :: Int  -- Provides offsets into the global lookup table.
                        } deriving (Show, Eq)

makeLenses ''CodeGenState

data InstEnum = PUSH_GLOBAL | PUSH_INT | PUSH | MKAP | SLIDE | UNWIND deriving (Show, Eq, Enum)

data NodeEnum = NODE_INT | NODE_AP | NODE_GLOBAL deriving (Show, Eq, Enum)

instructionType :: LLVM.Type
instructionType = LLVM.StructureType True [ LLVM.IntegerType 32  -- Instruction identifier (see 'InstEnum').
                                          , LLVM.IntegerType 32  -- Optional integer argument (push and slide instructions).
                                          ]

nodeRefType :: LLVM.Type
nodeRefType = LLVM.StructureType True [ LLVM.IntegerType 32  -- Node type identifier (see 'NodeEnum').
                                      , LLVM.PointerType (LLVM.IntegerType 8) (LLVM.AddrSpace 0)  -- Pointer to real node.
                                      ]

-- This doesn't need to be a struct and may be changed in future.
nodeIntType :: LLVM.Type
nodeIntType = LLVM.StructureType True [ LLVM.IntegerType 32  -- Integer value.
                                      ]

nodeApType :: LLVM.Type
nodeApType = LLVM.StructureType True [ LLVM.PointerType (nodeRefType) (LLVM.AddrSpace 0)  -- Left of application. Expected to be a function.
                                     , LLVM.PointerType (nodeRefType) (LLVM.AddrSpace 0)  -- Right of application. Can be anything.
                                     ]

nodeGlobalType :: LLVM.Type
nodeGlobalType = LLVM.StructureType True [ LLVM.IntegerType 32  -- Arity of global.
                                         , LLVM.PointerType (instructionType) (LLVM.AddrSpace 0)  -- Pointer to start of instructions.
                                         ]

initState :: CodeGenState
initState = CGS { _globalIdNext = 0  -- Start at offset 0.
                }

-- | Get the next global id (offset) and increment the tracker.
newGlobalId :: State CodeGenState Int
newGlobalId = do
    id <- use globalIdNext
    globalIdNext .= id+1
    pure id

generateIR :: [(String, [Instruction String], Int)] -> LLVM.Module
generateIR globals = LLVM.Module "MyModule" Nothing Nothing decls
    where
        decls = execWriter $ mapM_ initGlobal globals

initGlobal :: (String, [Instruction String], Int) -> Writer [LLVM.Definition] ()
initGlobal (name, code, arity) = do
    initGlobalInstructions code $ LLVM.Name (name ++ "_instructions")
    let llvmName = LLVM.Name name
        global = LLVM.GlobalVariable { LG.name = llvmName
                                     , LG.linkage = LLVM.Internal
                                     , LG.visibility = LLVM.Default
                                     , LG.isThreadLocal = True
                                     , LG.addrSpace = LLVM.AddrSpace 0
                                     , LG.hasUnnamedAddr = True
                                     , LG.isConstant = True
                                     , LG.type' = nodeGlobalType
                                     , LG.initializer = Just $ initGlobalNode arity $ LLVM.Name (name ++ "_instructions")
                                     , LG.section = Nothing
                                     , LG.alignment = 0
                                     }
    tell [LLVM.GlobalDefinition global]

initGlobalInstructions :: [Instruction String] -> LLVM.Name -> Writer [LLVM.Definition] ()
initGlobalInstructions is name = do
    let decl = LLVM.GlobalVariable { LG.name = name
                                   , LG.linkage = LLVM.Internal
                                   , LG.visibility = LLVM.Default
                                   , LG.isThreadLocal = True
                                   , LG.addrSpace = LLVM.AddrSpace 0
                                   , LG.hasUnnamedAddr = True
                                   , LG.isConstant = True
                                   , LG.type' = LLVM.ArrayType (fromIntegral $ length is) instructionType
                                   , LG.initializer = Just $ LLVM.Array instructionType $ map mkMachineInst is
                                   , LG.section = Nothing
                                   , LG.alignment = 0
                                   }
    tell [LLVM.GlobalDefinition decl]

mkMachineInst :: Instruction String -> LLVM.Constant
mkMachineInst (PushGlobal name) = LLVM.Struct True [ LLVM.Int 32 $ toInteger $ fromEnum PUSH_GLOBAL
                                                   , LLVM.Int 32 0
                                                   ]
mkMachineInst (PushInt n) = LLVM.Struct True [ LLVM.Int 32 $ toInteger $ fromEnum PUSH_INT
                                             , LLVM.Int 32 $ toInteger n
                                             ]
mkMachineInst (Push n) = LLVM.Struct True [ LLVM.Int 32 $ toInteger $ fromEnum PUSH
                                          , LLVM.Int 32 $ toInteger n
                                          ]
mkMachineInst MkAp = LLVM.Struct True [ LLVM.Int 32 $ toInteger $ fromEnum MKAP
                                      , LLVM.Int 32 0
                                      ]
mkMachineInst (Slide n) = LLVM.Struct True [ LLVM.Int 32 $ toInteger $ fromEnum SLIDE
                                           , LLVM.Int 32 $ toInteger n
                                           ]
mkMachineInst Unwind = LLVM.Struct True [ LLVM.Int 32 $ toInteger $ fromEnum UNWIND
                                        , LLVM.Int 32 $ toInteger 0
                                        ]

initGlobalNode :: Int -> LLVM.Name -> LLVM.Constant
initGlobalNode arity name = LLVM.Struct True [LLVM.Int 32 (toInteger arity), LC.GetElementPtr True (LLVM.GlobalReference name) [LLVM.Int 32 0, LLVM.Int 32 0]]
