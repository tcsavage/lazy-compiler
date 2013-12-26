{- Implements a G-Machine interpreter as outlined in "Implementing Functional Languages a tutorial" Peyton Jones, Lester (2000). -}

module GMachine.Interpreter where

import Control.Applicative
import Control.Monad
import qualified Data.Map as M
import Data.Maybe
import Data.Unique
import Text.Printf
import Text.Show.Pretty (ppShow)

import GMachine

type Name = String
type GMCode = [Instruction Name]
type GMStack = [Addr]
type Addr = Unique
type GMHeap = M.Map Addr Node
type GMGlobals = M.Map Name Addr

-- | Represents the state of the machine in a given step.
data GMState = GMState { gmInst :: GMCode
                       , gmStack :: GMStack
                       , gmHeap :: GMHeap
                       , gmGlobals :: GMGlobals  -- Global lookup table
                       } deriving (Show, Eq)

-- | Objects allocated on the "heap".
data Node = NNum Int  -- Value
          | NAp Addr Addr  -- Addr0 must be a function
          | NGlobal Int GMCode  -- Arity and instructions
          | NInd Addr  -- Indirection node
          deriving (Show, Eq)

-- | Lets us pretty print `Addr` values when inspecting state.
instance Show Unique where
    show = show . hashUnique

mkState :: [(Name, GMCode, Int)] -> IO GMState
mkState globals = do
    (globalTable, heap) <- initGlobals globals
    let mainF = findMain globals
    pure $ GMState mainF [] (M.fromList heap) (M.fromList globalTable)

findMain :: [(Name, GMCode, Int)] -> GMCode
findMain [] = error "Main function not found."
findMain (("main", code, arity):_) = code
findMain (_:rest) = findMain rest

initGlobals :: [(Name, GMCode, Int)] -> IO ([(Name, Addr)], [(Addr, Node)])
initGlobals decls = unzip <$> mapM initGlobal decls

initGlobal :: (Name, GMCode, Int) -> IO ((Name, Addr), (Addr, Node))
initGlobal (name, code, arity) = do
    addr <- newUnique
    pure ((name, addr), (addr, NGlobal arity code))

-- | Returns 'True' if the state is final.
stateFinal :: GMState -> Bool
stateFinal = null . gmInst

-- | Push an address onto the stack.
stackPush :: Addr -> GMState -> GMState
stackPush addr s0 = s0 { gmStack = addr:stack0 }
    where
        stack0 = gmStack s0

-- | Pop.
stackPop :: GMState -> (Addr, GMState)
stackPop s0 = (head stack0, s0 { gmStack = drop 1 stack0 })
    where
        stack0 = gmStack s0

-- | Pop many.
stackPopMany :: Int -> GMState -> GMState
stackPopMany n s0 = s0 { gmStack = drop n stack0 }
    where
        stack0 = gmStack s0

-- | Peek.
stackPeek :: GMState -> Addr
stackPeek s0 = top
    where
        (top:_) = gmStack s0

-- | Apply a function to the head of a list only.
appHead :: (a -> a) -> [a] -> [a]
appHead _ [] = []
appHead f (x:xs) = f x : xs

-- | Stack rewrite.
stackRewrite :: Addr -> Int -> GMState -> GMState
stackRewrite addr pos s0 = s0 { gmStack = top ++ appHead (const addr) btm }
    where
      (top, btm) = splitAt pos $ gmStack s0

-- | Return the heap address of a global.
getGlobal :: Name -> GMState -> Addr
getGlobal name = fromMaybe (error $ printf "Couldn't find global `%s`" name) . M.lookup name . gmGlobals

-- | "Allocate" a node on the "heap".
hAlloc :: Node -> GMState -> IO (Addr, GMState)
hAlloc node s0 = do
    addr <- newUnique
    pure (addr, s0 { gmHeap = M.insert addr node heap0 })
  where
    heap0 = gmHeap s0

-- | Lookup an node on the "heap" by its address.
hDeref :: Addr -> GMState -> Node
hDeref addr = fromMaybe (error "Failed to dereference heap address.") . M.lookup addr . gmHeap

-- | Helper function for getting the address of the nth argument.
getArg :: Node -> Addr
getArg (NAp f x) = x
getArg _ = error "Tried to get argument of non-application node."

-- | Run the machine witht he given initial state. Produce a list of states.
eval :: GMState -> IO [GMState]
eval state0 = (state0 :) <$> states
    where
        states = if stateFinal state0 then pure [] else (eval =<< step state0)

-- | Run the machine with the given initial state and print each state at each step.
eval_ :: GMState -> IO ()
eval_ s0
    | stateFinal s0 = putStrLn "Done."
    | otherwise = do
        s1 <- step s0
        putStrLn $ ppShow s1
        eval_ s1

-- | Run the machine with the given initial state and return its final value (or 'Nothing' if not an integer node.)
evalToValue :: GMState -> IO (Maybe Int)
evalToValue s0 = do
    sn <- last <$> eval s0
    pure $ do
        head <- listToMaybe $ gmStack sn
        (NNum r) <- M.lookup head $ gmHeap sn
        pure r

-- | Step the machine.
step :: GMState -> IO GMState
step s0 = dispatch i (s0 { gmInst = is })
    where
        (i:is) = gmInst s0

-- | Select the instruction handler function.
dispatch :: Instruction Name -> GMState -> IO GMState
dispatch (PushGlobal name) = pushGlobal name
dispatch (PushInt n) = pushInt n
dispatch (Push x) = push x
dispatch MkAp = mkAp
dispatch (Update n) = update n
dispatch (Pop n) = pop n
dispatch Unwind = unwind

-- | Lookup the address of the given global and push it onto the stack.
pushGlobal :: Name -> GMState -> IO GMState
pushGlobal name s0 = pure $ stackPush (getGlobal name s0) s0

-- | Allocate a new integer node on the heap and push its address onto the stack.
pushInt :: Int -> GMState -> IO GMState
pushInt n s0 = do
    (addr, s1) <- hAlloc (NNum n) s0
    pure $ stackPush addr s1

-- | Push the address of the nth argument onto the stack.
push :: Int -> GMState -> IO GMState
push x s0 = pure $ stackPush (getArg $ hDeref (stack0 !! (x+1)) s0) s0
    where
        stack0 = gmStack s0

-- | Pop the tow two addresses from the stack. Allocate a new application node on the heap using those two addresses, and push the new node's adderss onto the stack.
mkAp :: GMState -> IO GMState
mkAp s0 = do
    let (x1:x2:xs) = gmStack s0
    (addr, s1) <- hAlloc (NAp x1 x2) s0
    pure $ s1 { gmStack = addr:xs }

-- | Pop the top address from the stack and call it @x@. Pop the next @n@ items on the stack and discard them. Push @x@ back onto the stack.
slide :: Int -> GMState -> IO GMState
slide n s0 = pure $ s0 { gmStack = x : drop n xs }
    where
        (x:xs) = gmStack s0

update :: Int -> GMState -> IO GMState
update 0 s0 = pure s0
update n s0 = do
    let (top, s1) = stackPop s0
    (addr, s2) <- hAlloc (NInd top) s1
    pure $ stackRewrite addr n s2

pop :: Int -> GMState -> IO GMState
pop 0 = pure
pop n = pure . stackPopMany n

-- | Traverse the graph's application nodes from HEAD and push their addresses onto the stack. Stop at the first global and prepare it to run.
unwind :: GMState -> IO GMState
unwind state = pure $ newState $ hDeref a state
    where
        (a:as) = gmStack state
        newState (NNum n) = state
        newState (NAp f x) = state { gmInst = [Unwind], gmStack = f:a:as }
        newState (NGlobal n code)
            | length as < n = error "Unwinding with too few arguments."
            | otherwise = state { gmInst = code }
        newState (NInd addr) = state { gmInst = [Unwind], gmStack = addr:as }
            where
                (a:as) = gmStack state
