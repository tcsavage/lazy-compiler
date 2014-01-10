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
                   | Pack Int Int
                   | CaseJump [(Int, [Instruction a])]
                   | Split Int
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
    fmap _ (Pack t a) = Pack t a
    fmap f (CaseJump x) = CaseJump $ map (\(t, code) -> (t, map (fmap f) code)) x
    fmap _ (Split n) = Split n

-- | The first code to be loaded into the automaton.
initialCode :: [Instruction String]
initialCode = [PushGlobal "main", Eval]
