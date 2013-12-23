{- Specifies a "G-Machine" graph reduction machine and implements an interpreter -}

module Main where

import Prelude hiding (init)

import Control.Applicative
import Control.Monad
import qualified Data.Map as M
import Data.Maybe
import Data.Unique
import Text.Printf
import Text.Show.Pretty (ppShow)

type Name = String
type GMCode = [Instruction]
type GMStack = [Addr]
type Addr = Unique
type GMHeap = M.Map Addr Node
type GMGlobals = M.Map Name Addr

data GMState = GMState { gmInst :: GMCode
                       , gmStack :: GMStack
                       , gmHeap :: GMHeap
                       , gmGlobals :: GMGlobals  -- Global lookup table
                       } deriving (Show, Eq)

data Instruction = PushGlobal Name
                 | PushInt Int
                 | Push Int
                 | MkAp
                 | Slide Int
                 | Unwind
                 deriving (Show, Eq)

data Node = NNum Int  -- Value
          | NAp Addr Addr  -- Addr0 must be a function
          | NGlobal Int GMCode  -- Arity and instructions
          deriving (Show, Eq)

instance Show Unique where
    show = show . hashUnique

sampS :: GMCode
sampS = [Push 2, Push 2, MkAp, Push 3, Push 2, MkAp, MkAp, Slide 4, Unwind]

sampK :: GMCode
sampK = [Push 0, Slide 3, Unwind]

sampI :: GMCode
sampI = [Push 0, Slide 2, Unwind]

sampMain :: GMCode
sampMain = [PushInt 3, PushGlobal "I", PushGlobal "K", PushGlobal "S", MkAp, MkAp, MkAp, Slide 1, Unwind]

sampGlobals :: [(Name, GMCode, Int)]
sampGlobals = [("S", sampS, 2), ("K", sampK, 2), ("I", sampI, 1)]

sampInitState :: IO GMState
sampInitState = do
    (globalTable, heap) <- initGlobals sampGlobals
    pure $ GMState sampMain [] (M.fromList heap) (M.fromList globalTable)

initGlobals :: [(Name, GMCode, Int)] -> IO ([(Name, Addr)], [(Addr, Node)])
initGlobals decls = unzip <$> mapM initGlobal decls

initGlobal :: (Name, GMCode, Int) -> IO ((Name, Addr), (Addr, Node))
initGlobal (name, code, arity) = do
    addr <- newUnique
    pure ((name, addr), (addr, NGlobal arity code))

stateFinal :: GMState -> Bool
stateFinal = null . gmInst

stackPush :: Addr -> GMState -> GMState
stackPush addr s0 = s0 { gmStack = addr:stack0 }
    where
        stack0 = gmStack s0

getGlobal :: Name -> GMState -> Addr
getGlobal name = fromMaybe (error $ printf "Couldn't find global `%s`" name) . M.lookup name . gmGlobals

hAlloc :: Node -> GMState -> IO (Addr, GMState)
hAlloc node s0 = do
    addr <- newUnique
    pure (addr, s0 { gmHeap = M.insert addr node heap0 })
  where
    heap0 = gmHeap s0

hDeref :: Addr -> GMState -> Node
hDeref addr = fromMaybe (error "Failed to dereference heap address.") . M.lookup addr . gmHeap

getArg :: Node -> Addr
getArg (NAp f x) = x
getArg _ = error "Tried to get argument of non-application node."

eval :: GMState -> IO [GMState]
eval state0 = (state0 :) <$> states
    where
        states = if stateFinal state0 then pure [] else (eval =<< step state0)

eval_ :: GMState -> IO ()
eval_ s0
    | stateFinal s0 = putStrLn "Done."
    | otherwise = do
        s1 <- step s0
        putStrLn $ ppShow s1
        eval_ s1

step :: GMState -> IO GMState
step s0 = dispatch i (s0 { gmInst = is })
    where
        (i:is) = gmInst s0

dispatch :: Instruction -> GMState -> IO GMState
dispatch (PushGlobal name) = pushGlobal name
dispatch (PushInt n) = pushInt n
dispatch (Push x) = push x
dispatch MkAp = mkAp
dispatch (Slide n) = slide n
dispatch Unwind = unwind

pushGlobal :: Name -> GMState -> IO GMState
pushGlobal name s0 = pure $ stackPush (getGlobal name s0) s0

pushInt :: Int -> GMState -> IO GMState
pushInt n s0 = do
    (addr, s1) <- hAlloc (NNum n) s0
    pure $ stackPush addr s1

push :: Int -> GMState -> IO GMState
push x s0 = pure $ stackPush (getArg $ hDeref (stack0 !! (x+1)) s0) s0
    where
        stack0 = gmStack s0

mkAp :: GMState -> IO GMState
mkAp s0 = do
    let (x1:x2:xs) = gmStack s0
    (addr, s1) <- hAlloc (NAp x1 x2) s0
    pure $ s1 { gmStack = addr:xs }

slide :: Int -> GMState -> IO GMState
slide n s0 = pure $ s0 { gmStack = x : drop n xs }
    where
        (x:xs) = gmStack s0

unwind :: GMState -> IO GMState
unwind state = pure $ newState $ hDeref a state
    where
        (a:as) = gmStack state
        newState (NNum n) = state
        newState (NAp f x) = state { gmInst = [Unwind], gmStack = f:a:as }
        newState (NGlobal n code)
            | length as < n = error "Unwinding with too few arguments."
            | otherwise = state { gmInst = code }
