module STG.AST where

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
