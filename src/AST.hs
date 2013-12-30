{-# LANGUAGE TemplateHaskell #-}

module AST where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable as F
import Data.List
import Data.Monoid
import Data.Traversable as T
import Prelude.Extras

infixl 9 :@
infixr 9 ~>

data Expr a = V a  -- Variable
            | L Int  -- Integer literal
            | Expr a :@ Expr a  -- Application
            | Lam a (Expr a)  -- Lambda
            | Let Bool [(Ident, Expr a)] (Expr a)  -- Recursive let binding
            | PrimFun PrimFun  -- Primitive function
            deriving (Eq,Ord,Show,Read)

data Type = TyFun Type Type
          | TyInt
          deriving (Eq,Ord,Show,Read)

(~>) :: Type -> Type -> Type
(~>) = TyFun

data Decl = Decl { dident :: Ident, dval :: Expr Ident } deriving (Show, Eq)

unDecl :: Decl -> (Ident, Expr Ident)
unDecl (Decl i e) = (i, e)

data Ident = Id { name :: String
                , typeOf :: Type
                } deriving (Eq,Ord,Show,Read)

data PrimFun = PrimBinOp PrimBinOp
             deriving (Eq,Ord,Show,Read)

data PrimBinOp = PrimAdd
               | PrimMul
               deriving (Eq,Ord,Show,Read)

evalPrimBinOp :: PrimBinOp -> Int -> Int -> Expr a
evalPrimBinOp PrimAdd l r = L (l + r)

getArity :: Expr a -> Int
getArity (Lam b expr) = 1 + (getArity expr)
getArity (PrimFun op) = case op of
    PrimBinOp _ -> 2
getArity _ = 0

-- Module type.
data Module = Module { _modName :: String
                     , _tlDecls :: [(Ident, Expr Ident)]
                     } deriving (Show)

makeLenses ''Module

getTopLevelNames :: Module -> [String]
getTopLevelNames = nub . map (name . fst) . _tlDecls

testMain :: Expr Ident
testMain = Lam id_in $ V id_add :@ L 5 :@ V id_in
    where
        id_in = Id "in" TyInt
        id_add = Id "add" (TyInt ~> TyInt ~> TyInt)

testMain2 :: Expr Ident
testMain2 = V id_s :@ V id_k :@ V id_i :@ L 3
    where
        id_i = Id "i" (TyInt ~> TyInt)
        id_k = Id "k" (TyInt ~> TyInt ~> TyInt)
        id_s = Id "s" ((TyInt ~> TyInt ~> TyInt) ~> (TyInt ~> TyInt) ~> TyInt ~> TyInt)

i = Lam (Id "x" TyInt) $ V (Id "x" TyInt)
k = Lam (Id "x" TyInt) $ Lam (Id "y" TyInt) $ V (Id "x" TyInt)
s = Lam id_x $ Lam id_y $ Lam id_z $ V id_x :@ V id_z :@ (V id_y :@ V id_z)
    where
        id_x = Id "x" (TyInt ~> TyInt ~> TyInt)
        id_y = Id "y" (TyInt ~> TyInt)
        id_z = Id "z" TyInt

testTopLevels :: [(Ident, Expr Ident)]
testTopLevels = [(id_id, i), (id_const, k), (id_s, s), (id_main, testMain2)]
    where
        id_id = Id "i" (TyInt ~> TyInt)
        id_const = Id "k" (TyInt ~> TyInt ~> TyInt)
        id_s = Id "s" ((TyInt ~> TyInt ~> TyInt) ~> (TyInt ~> TyInt) ~> TyInt ~> TyInt)
        id_main = Id "main" (TyInt)

testMod :: Module
testMod = Module "Test" [(id_id, i), (id_const, k), (id_main, testMain), (id_add, PrimFun $ PrimBinOp PrimAdd)]
    where
        id_id = Id "id" (TyInt ~> TyInt)
        id_add = Id "add" (TyInt ~> TyInt ~> TyInt)
        id_main = Id "main" (TyInt ~> TyInt)
        id_const = Id "const" (TyInt ~> TyInt ~> TyInt)
