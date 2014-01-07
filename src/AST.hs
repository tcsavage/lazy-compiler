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

infixl 9 :@
infixr 9 ~>

data Expr a = V a  -- Variable
            | L Int  -- Integer literal
            | Expr a :@ Expr a  -- Application
            | Lam a (Expr a)  -- Lambda
            | Let Bool [(Ident, Expr a)] (Expr a)  -- Recursive let binding
            | Constr Int Type  -- Data constructor (tag and type)
            | PrimFun PrimFun  -- Primitive function
            deriving (Eq,Ord,Show,Read)

data Type = TyFun Type Type
          | TyInt
          | TyVar Ident
          | TyKindStar
          deriving (Eq,Ord,Show,Read)

(~>) :: Type -> Type -> Type
(~>) = TyFun

data Decl = DTerm { dident :: Ident, dval :: Expr Ident }
          | DType { dident :: Ident, dconstrs :: [Ident] }
          deriving (Show, Eq)

--unDecl :: Decl -> (Ident, Expr Ident)
--unDecl (Decl i e) = (i, e)

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
                     , _tlDecls :: [Decl]
                     } deriving (Show)

makeLenses ''Module

--getTopLevelNames :: Module -> [String]
--getTopLevelNames = nub . map (name . dval) . _tlDecls

returnType :: Type -> Type
returnType (TyFun _ r) = returnType r
returnType x = x
