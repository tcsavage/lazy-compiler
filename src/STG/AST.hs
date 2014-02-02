module STG.AST where

-- The following is the definition of the STG language.

type Var = String

data Program = Program [Binding] deriving (Show, Eq)

data Binding = Binding Var Lambda deriving (Show, Eq)

data Lambda = Lambda [Var] UpdateFlag [Var] Expr deriving (Show, Eq)

data UpdateFlag = U | N deriving (Show, Eq)

data Expr = Let [Binding] Expr
          | LetRec [Binding] Expr
          | Case Expr Alts
          | Ap Var [Atom]
          | Constr Constr [Atom]
          | Prim Prim [Atom]
          | Literal Literal
          deriving (Show, Eq)

type Constr = Int

data Alts = Algebraic [AAlt] DefAlt
          | Primitive [PAlt] DefAlt
          deriving (Show, Eq)

data AAlt = AAlt Constr [Var] Expr deriving (Show, Eq)

data PAlt = PAlt Literal Expr deriving (Show, Eq)

data DefAlt = DefBinding Var Expr
            | Default Expr
            deriving (Show, Eq)

type Literal = Int

data Prim = PrimAdd | PrimMul deriving (Show, Eq)

data Atom = AtomVar Var
          | AtomLit Literal
          deriving (Show, Eq)

mul :: Binding
mul = Binding "mul" (Lambda [] N ["x", "y"] (Prim PrimMul [AtomVar "x", AtomVar "y"]))

double :: Binding
double = Binding "double" (Lambda [] N [] (Ap "mul" [AtomLit 2]))

stgmain1 :: Binding
stgmain1 = Binding "main" (Lambda [] N [] (Let [Binding "x" (Lambda [] N [] (Ap "double" [AtomLit 2])), Binding "y" (Lambda ["x"] N [] (Ap "double" [AtomVar "x"]))] (Ap "dumpInt" [AtomVar "y"])))

stgmodule1 :: Program
stgmodule1 = Program [mul, double, stgmain1]

stgmain2 :: Binding
stgmain2 = Binding "main" (Lambda [] N [] (Let [Binding "x" (Lambda [] N [] (Ap "double" [AtomLit 0]))] (Case (Ap "x" []) (Primitive [PAlt 0 (Ap "dumpInt" [AtomLit 9000])] (DefBinding "z" (Ap "dumpInt" [AtomVar "z"]))))))

stgmodule2 :: Program
stgmodule2 = Program [mul, double, stgmain2]

stgmain3 :: Binding
stgmain3 = Binding "main" (Lambda [] N [] (Let [Binding "x" (Lambda [] N [] (Ap "double" [AtomLit 8])), Binding "y" (Lambda ["x"] N [] (Constr 0 [AtomVar "x"]))] (Case (Ap "y" []) (Algebraic [AAlt 0 ["foo"] (Ap "dumpInt" [AtomVar "foo"])] (Default (Ap "dumpInt" [AtomLit 1337]))))))

stgmodule3 :: Program
stgmodule3 = Program [mul, double, stgmain3]
