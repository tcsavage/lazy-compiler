{-# LANGUAGE PackageImports #-}
{- Implements a G-Machine interpreter as outlined in "Implementing Functional Languages a tutorial" Peyton Jones, Lester (2000). -}

module GMachine.Interpreter where

import Control.Applicative
import Control.Monad
import "mtl" Control.Monad.State
import qualified Data.Map as M
import Data.Maybe
import Data.Unique
import qualified System.IO.Unsafe
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
                       , gmDump :: [(GMCode, GMStack)]
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

-- | State computation over GMState in IO monad.
type GM a = StateT GMState IO a

runGM :: GM a -> GMState -> IO (a, GMState)
runGM = runStateT

evalGM :: GM a -> GMState -> IO a
evalGM = evalStateT

execGM :: GM a -> GMState -> IO GMState
execGM = execStateT

-- Grap a heap address to represent NULL. Doesn't matter what it is.
heapNull :: Addr
heapNull = System.IO.Unsafe.unsafePerformIO newUnique

mkState :: [(Name, GMCode, Int)] -> IO GMState
mkState globals = do
    (globalTable, heap) <- initGlobals globals
    pure $ GMState initialCode [] [] (M.fromList heap) (M.fromList globalTable)

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

isFinal :: GM Bool
isFinal = fmap stateFinal get

setCode :: [Instruction Name] -> GM ()
setCode code = do
    s0 <- get
    put $ s0 { gmInst = code }

-- | Push an address onto the stack.
stackPush :: Addr -> GM ()
stackPush addr = do
    s0 <- get
    let stack0 = gmStack s0
    put $ s0 { gmStack = addr:stack0 }

-- | Pop.
stackPop :: GM Addr
stackPop = do
    s0 <- get
    let (a:as) = gmStack s0
    put $ s0 { gmStack = as }
    pure a

-- | Pop many.
stackPopMany :: Int -> GM ()
stackPopMany n = do
    s0 <- get
    let stack0 = gmStack s0
    put $ s0 { gmStack = drop n stack0 }

-- | Peek.
stackPeek :: GM Addr
stackPeek = (head . gmStack) <$> get

-- | Get the current number of stack items.
stackSize :: GM Int
stackSize = fmap (length . gmStack) get

-- | Apply a function to the head of a list only.
appHead :: (a -> a) -> [a] -> [a]
appHead _ [] = []
appHead f (x:xs) = f x : xs

-- | Stack rewrite.
stackRewrite :: Addr -> Int -> GM ()
stackRewrite addr pos = do
    s0 <- get
    let (top, btm) = splitAt pos $ gmStack s0
    put $ s0 { gmStack = top ++ appHead (const addr) btm }

-- | Get number of dump items.
dumpSize :: GM Int
dumpSize = (length . gmDump) <$> get

-- | Restore stack and code from top of dump, keeping current stack head.
dumpRestore :: GM ()
dumpRestore = do
    s0 <- get
    let d = gmDump s0
    case d of
        [] -> pure ()
        (i', s'):ds -> do
            h <- stackPop
            put $ s0 { gmInst = i', gmStack = h:s', gmDump = ds }

-- | Push current instructions and stack onto the dump.
dumpPush :: GM ()
dumpPush = do
    s0 <- get
    let i = gmInst s0
    let (a:s) = gmStack s0
    let d = gmDump s0
    put $ s0 { gmInst = [Unwind], gmStack = [a], gmDump = (i, s):d }

-- | Return the heap address of a global.
getGlobal :: Name -> GM Addr
getGlobal name = (fromMaybe (error $ printf "Couldn't find global `%s`" name) . M.lookup name . gmGlobals) <$> get

-- | "Allocate" a node on the "heap".
hAlloc :: Node -> GM Addr
hAlloc node = do
    addr <- liftIO newUnique
    s0 <- get
    let heap0 = gmHeap s0
    put $ s0 { gmHeap = M.insert addr node heap0 }
    pure addr

-- | Lookup an node on the "heap" by its address.
hDeref :: Addr -> GM Node
hDeref addr = (fromMaybe (error "Failed to dereference heap address.") . M.lookup addr . gmHeap) <$> get

-- | Helper function for getting the address of the nth argument.
getArg :: Node -> Addr
getArg (NAp f x) = x
getArg _ = error "Tried to get argument of non-application node."

-- | Rearrange the stack.
rearrange :: Int -> GM ()
rearrange n = do
    s0 <- get
    let heap = gmHeap s0
    let as = gmStack s0
    as' <- mapM (fmap getArg . hDeref) $ tail as
    put $ s0 { gmStack = take n as' ++ drop n as }

-- | Create an Int node and push its address onto the stack.
boxInteger :: Int -> GM ()
boxInteger n = do
    addr <- hAlloc $ NNum n
    stackPush addr

-- | Extract int value from the given address of an int node.
unboxInteger :: Addr -> GM Int
unboxInteger addr = do
    node <- hDeref addr
    case node of
        NNum n -> pure n
        _ -> error "Tried to unbox non-int node."

-- | Run unary operator.
primOp1 :: (b -> GM ()) -> (Addr -> GM a) -> (a -> b) -> GM ()
primOp1 boxer unboxer op = do
    a <- unboxer =<< stackPop
    boxer $ op a

-- | Run binary operator.
primOp2 :: (b -> GM ()) -> (Addr -> GM a) -> (a -> a -> b) -> GM ()
primOp2 boxer unboxer op = do
    a <- unboxer =<< stackPop
    b <- unboxer =<< stackPop
    boxer $ op a b

-- | Primitive unary arithmetic operator.
primArith1 :: (Int -> Int) -> GM ()
primArith1 = primOp1 boxInteger unboxInteger

-- | Primitive binary arithmetic operator.
primArith2 :: (Int -> Int -> Int) -> GM ()
primArith2 = primOp2 boxInteger unboxInteger

-- | Run the machine witht he given initial state. Produce a list of states.
runGCode :: GMState -> IO [GMState]
runGCode state0 = (state0 :) <$> states
    where
        states = if stateFinal state0 then pure [] else (runGCode =<< step state0)

-- | Run the machine with the given initial state and print each state at each step.
runGCodeVerbose :: GMState -> IO ()
runGCodeVerbose s0
    | stateFinal s0 = putStrLn "Done."
    | otherwise = do
        s1 <- step s0
        putStrLn $ ppShow s1
        runGCodeVerbose s1

-- | Run the machine with the given initial state and return its final value (or 'Nothing' if not an integer node.)
runGCodeToValue :: GMState -> IO (Maybe Int)
runGCodeToValue s0 = do
    sn <- last <$> runGCode s0
    pure $ do
        head <- listToMaybe $ gmStack sn
        (NNum r) <- M.lookup head $ gmHeap sn
        pure r

-- | Step the machine.
step :: GMState -> IO GMState
step s0 = execGM (dispatch i) (s0 { gmInst = is })
    where
        (i:is) = gmInst s0

-- | Select the instruction handler function.
dispatch :: Instruction Name -> GM ()
dispatch (PushGlobal name) = pushGlobal name
dispatch (PushInt n) = pushInt n
dispatch (Push x) = push x
dispatch MkAp = mkAp
dispatch (Slide n) = slide n
dispatch (Alloc n) = alloc n
dispatch (Update n) = update n
dispatch (Pop n) = pop n
dispatch Unwind = unwind
dispatch Eval = eval
dispatch Add = primArith2 (+)
dispatch Mul = primArith2 (*)

-- | Lookup the address of the given global and push it onto the stack.
pushGlobal :: Name -> GM ()
pushGlobal name = do
    g <- getGlobal name
    stackPush g

-- | Allocate a new integer node on the heap and push its address onto the stack.
pushInt :: Int -> GM ()
pushInt n = do
    addr <- hAlloc $ NNum n
    stackPush addr

-- | Push the address of the nth argument onto the stack.
push :: Int -> GM ()
push x = do
    stack0 <- gmStack <$> get
    stackPush (stack0 !! x)

-- | Pop the tow two addresses from the stack. Allocate a new application node on the heap using those two addresses, and push the new node's adderss onto the stack.
mkAp :: GM ()
mkAp = do
    x1 <- stackPop
    x2 <- stackPop
    addr <- hAlloc (NAp x1 x2)
    stackPush addr

-- | Pop the top address from the stack and call it @x@. Pop the next @n@ items on the stack and discard them. Push @x@ back onto the stack.
slide :: Int -> GM ()
slide n = do
    x <- stackPop
    stackPopMany n
    stackPush x

alloc :: Int -> GM ()
alloc n = replicateM_ n allocNodeAndPush
    where
        allocNodeAndPush = do
            addr <- hAlloc $ NInd heapNull
            stackPush addr

update :: Int -> GM ()
update 0 = pure ()
update n = do
    top <- stackPop
    addr <- hAlloc $ NInd top
    stackRewrite addr n

pop :: Int -> GM ()
pop 0 = pure ()
pop n = stackPopMany n

-- | Traverse the graph's application nodes from HEAD and push their addresses onto the stack. Stop at the first global and prepare it to run.
unwind :: GM ()
unwind = do
    top <- stackPeek
    node <- hDeref top
    newState node
    where
        newState (NNum n) = do
            ds <- dumpSize
            when (ds > 0) dumpRestore
        newState (NAp f x) = do
            stackPush f
            setCode [Unwind]
        newState (NGlobal n code) = do
            size <- stackSize
            case (size - 1) < n of
                True -> error "Unwinding with too few arguments."
                False -> do
                    rearrange n
                    setCode code
        newState (NInd addr) = do
            stackPop
            stackPush addr
            setCode [Unwind]

eval :: GM ()
eval = dumpPush
