{- Specifies an abstract "G-Machine" graph reduction machine. -}

module GMachine where

data Instruction a = PushGlobal a
                   | PushInt Int
                   | Push Int
                   | MkAp
                   | Slide Int
                   | Unwind
                   deriving (Show, Eq)
