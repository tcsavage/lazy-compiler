{- Specifies an abstract "G-Machine" graph reduction machine. -}

module GMachine where

-- | The set of G Machine instructions. Parameterised over the global name type.
data Instruction a = PushGlobal a
                   | PushInt Int
                   | Push Int
                   | MkAp
                   | Slide Int
                   | Alloc Int
                   | Update Int
                   | Pop Int
                   | Unwind
                   | Eval
                   | Add
                   | Mul
                   deriving (Show, Eq)

instance Functor Instruction where
    fmap f (PushGlobal name) = PushGlobal $ f name
    fmap _ (PushInt n) = (PushInt n)
    fmap _ (Push n) = (Push n)
    fmap _ MkAp = MkAp
    fmap _ (Slide n) = (Slide n)
    fmap _ (Alloc n) = (Alloc n)
    fmap _ (Update n) = (Update n)
    fmap _ (Pop n) = (Pop n)
    fmap _ Unwind = Unwind
    fmap _ Eval = Eval
    fmap _ Add = Add
    fmap _ Mul = Mul

-- | The first code to be loaded into the automaton.
initialCode :: [Instruction String]
initialCode = [PushGlobal "main", Eval]
