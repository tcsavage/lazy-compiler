{-# LANGUAGE PackageImports #-}

module GMTest where

import Control.Applicative
import Control.Monad
import "mtl" Control.Monad.Error
import qualified Data.Map as M
import System.Exit

--import qualified LLVM.General as FFI
--import qualified LLVM.General.Context as FFI

import GMachine
--import GMachine.Compiler
import GMachine.Interpreter

sampS :: GMCode
sampS = [Push 2, Push 2, MkAp, Push 3, Push 2, MkAp, MkAp, Slide 4, Unwind]

sampK :: GMCode
sampK = [Push 0, Slide 3, Unwind]

sampI :: GMCode
sampI = [Push 0, Slide 2, Unwind]

sampMain :: GMCode
sampMain = [PushInt 3, PushGlobal "I", PushGlobal "K", PushGlobal "S", MkAp, MkAp, MkAp, Slide 1, Unwind]

sampGlobals :: [(Name, GMCode, Int)]
sampGlobals = [("I", sampI, 1), ("K", sampK, 2), ("S", sampS, 2), ("main", sampMain, 0)]

sampInitState :: IO GMState
sampInitState = do
    (globalTable, heap) <- initGlobals sampGlobals
    pure $ GMState sampMain [] (M.fromList heap) (M.fromList globalTable)

--runErrorsOrQuit :: MonadIO m => ErrorT String m a -> m a
--runErrorsOrQuit comp = do
--    check <- runErrorT comp
--    case check of
--        Left err -> liftIO $ putStrLn err >> exitFailure
--        Right x -> return x

--main :: IO ()
--main = do
--    FFI.withContext $ \ctx -> do
--        runErrorsOrQuit $ FFI.withModuleFromAST ctx (generateIR sampGlobals) $ \mod' -> do
--            putStrLn =<< FFI.moduleString mod'
