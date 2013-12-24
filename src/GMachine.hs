{- Specifies an abstract "G-Machine" graph reduction machine. -}

module GMachine where

data Instruction a = PushGlobal a
                   | PushInt Int
                   | Push Int
                   | MkAp
                   | Slide Int
                   | Unwind
                   deriving (Show, Eq)

instance Functor Instruction where
    fmap f (PushGlobal name) = PushGlobal $ f name
    fmap _ (PushInt n) = (PushInt n)
    fmap _ (Push n) = (Push n)
    fmap _ MkAp = MkAp
    fmap _ (Slide n) = (Slide n)
    fmap _ Unwind = Unwind