module STG where

-- The following is the definition of the STG language.

type Var = String

data Program = Program [Binding]

data Binding = Binding Var Lambda

data Lambda = Lambda [Var] UpdateFlag [Var] Expr

data UpdateFlag = U | N

data Expr = Let [Binding] Expr
          | LetRec [Binding] Expr
          | Case Expr Alts
          | Ap Var [Atom]
          | Constr Constr [Atom]
          | Prim Prim [Atom]
          | Literal Literal

type Constr = Int

data Alts = Algebraic [AAlt] DefAlt
          | Primitive [PAlt] DefAlt

data AAlt = AAlt Constr [Var] Expr

data PAlt = PAlt Literal Expr

data DefAlt = DefBinding Var Expr
            | Default Expr

type Literal = Int

data Prim = PrimAdd | PrimMul

data Atom = AtomVar Var
          | AtomLit Literal

mul :: Binding
mul = Binding "mul" (Lambda [] N ["x", "y"] (Prim PrimMul [AtomVar "x", AtomVar "y"]))

double :: Binding
double = Binding "double" (Lambda [] N [] (Ap "mul" [AtomLit 2]))

stgmain1 :: Binding
stgmain1 = Binding "main" (Lambda [] N [] (Let [Binding "x" (Lambda [] N [] (Ap "double" [AtomLit 8]))] (Ap "dumpInt" [AtomVar "x"])))

stgmodule1 :: Program
stgmodule1 = Program [mul, double, stgmain1]

stgmain2 :: Binding
stgmain2 = Binding "main" (Lambda [] N [] (Let [Binding "x" (Lambda [] N [] (Ap "double" [AtomLit 0]))] (Case (Ap "x" []) (Primitive [PAlt 0 (Ap "dumpInt" [AtomLit 9000])] (Default (Ap "dumpInt" [AtomLit 1337]))))))

stgmodule2 :: Program
stgmodule2 = Program [mul, double, stgmain2]
